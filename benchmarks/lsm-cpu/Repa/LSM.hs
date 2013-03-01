{-# LANGUAGE FlexibleContexts #-}
module LSM where

import Data.List (intersperse)
import Control.Monad

import Prelude hiding ((++), reverse, take, map, zipWith, zipWith3, length)
import qualified Prelude

import qualified Data.Vector as DV
import qualified Data.Vector.Unboxed as UB
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Array.Repa
import RepaHelpers

import System.Exit
import System.Random.Mersenne.Pure64 (newPureMT, pureMT, PureMT)
import Random
import LinAlg

-- Simulation parameters
n_paths :: Int
n_paths = 20000
n_points :: Int
n_points = 252
reg = 2

-- American put option parameters
r,vol,s0,t,dt,df :: Double
r   = 0.03               -- short rate
vol = 0.2                -- volatility
s0  = 100.0               -- initial stock level
t   = 1.0                -- time - to - maturity
dt  = t/(fromIntegral n_points) -- length of time interval
df  = exp(-r*dt)         -- discount factor per time interval
k   = 100.0              -- strike price


rng :: PureMT -> Int -> Array U DIM1 Double
rng gen count =
  let normalsvec = normals gen (count `div` 2)
      at_vec = normalsvec UB.++ (UB.reverse $ UB.map (* (-1)) normalsvec)
  in fromUnboxed (Z:.count) at_vec


-- ^ Monadic unfoldr. Stops unfolding after N steps. Does not
-- include the seed in the result.
unfoldrNM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
unfoldrNM _ 0 seed = return []
unfoldrNM f n seed = do
  x <- f seed
  xs <- unfoldrNM f (n-1) x
  return (x:xs)

-- ^ Generate paths
genPaths :: PureMT -> Int -> Int -> IO (DV.Vector (Array U DIM1 Double))
genPaths gen m n = do
   initvec <- computeUnboxedP $ fromFunction (Z :. n) (const s0)
   (DV.fromList . Prelude.map fst) `fmap` unfoldrNM iter m (initvec,0)
  where
    coef1 = dt*(r-0.5*vol*vol)
    coef2 = vol*sqrt(dt)

    normalsvec = rng gen (m*n)

    iter :: (Array U DIM1 Double, Int) -> IO (Array U DIM1 Double, Int)
    iter (prev, i) = do
      let ran = extract (Z :. i*n) (Z :. n) normalsvec
      x <- computeUnboxedP $ zipWith (\s x -> s * exp(coef1 + x*coef2)) prev ran
      return (x, i+1)

iv :: Array U DIM1 Double -> Array D DIM1 Double
iv arr = map (\x -> max (k-x) 0) arr

average :: Source r Double => Array r DIM1 Double -> Double
average xs = sumAllS xs / (fromIntegral $ length xs)

variance :: Source r Double => Array r DIM1 Double -> Double
variance xs = average (map (\x -> (avg-x)^2) xs)
  where avg = average xs

std :: Source r Double => Array r DIM1 Double -> Double
std = sqrt . variance

pick :: (Unbox a, Monad m, Source r a) => Array D DIM1 Bool -> Array r DIM1 a -> m (Array U DIM1 a)
pick p xs = selectP (index p . index1) (index xs . index1) (length xs)

lsm :: Int -> Int -> IO Double
lsm n_points n_paths = do
  gen <- newPureMT
  s <- genPaths gen n_points n_paths
  init_disccashflow <- computeUnboxedP $ iv (DV.last s)
  disccashflow <- DV.foldM lsm' init_disccashflow (DV.reverse $ DV.init (DV.tail s))
  let v0 = df * average disccashflow
  let putprice = if k-s0 > v0 then k-s0 else v0
  return putprice
 where 
  exercise_decision :: Double -> Double -> Double -> Double
  exercise_decision ev iv v = if iv > 0 && iv > ev then iv else v
  {-# INLINE exercise_decision #-}
  lsm' :: Array U DIM1 Double -> Array U DIM1 Double -> IO (Array U DIM1 Double)
  lsm' disccashflow s = do
    let intrinsic_value = iv s
        p = map (>0) intrinsic_value
        default_out = map (*df) disccashflow
    y <- pick p default_out
    spick <- pick p s
    -- We should check that there are at least one left after "pick"
    rg <- polyfit spick y reg
    let estimatedtimevalue = polyvals rg (delay s)
    computeUnboxedP $ 
      zipWith3 exercise_decision 
               estimatedtimevalue
               intrinsic_value 
               default_out

-- main :: IO ()
-- main = do
--   disccashflow <- lsm n_points n_paths
--   let v0 = df * average disccashflow
--   let v0' = if k-s0 > v0 then k-s0 else v0
--   print v0'
--   print $ 1.96 * std(map (df*) disccashflow)/sqrt(fromIntegral $ length disccashflow)

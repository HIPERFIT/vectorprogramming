{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.List (intersperse)
import Control.Monad
import Control.DeepSeq

import Prelude hiding ((++), reverse, take, map, zipWith, length, zip)
import qualified Prelude

import qualified Data.Vector as DV
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Array.Repa
import RepaHelpers

import System.Random.Mersenne.Pure64 (newPureMT)
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


rng :: Int -> IO (Array U DIM1 Double)
rng n = do
  gen <- newPureMT
  return . fromUnboxed (Z:.n) $ normals gen n

-- ^ Monadic unfoldr. Stops unfolding after N steps and includes the
-- seed in the result.
unfoldrNM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
unfoldrNM _ 0 seed = return [seed]
unfoldrNM f n seed = do
  x <- f seed
  xs <- unfoldrNM f (n-1) x
  return (seed : xs)

-- ^ Generate paths
genPaths :: Int -> Int -> IO (DV.Vector (Array U DIM1 Double))
genPaths m n = do
   initvec <- computeP $ fromFunction (Z :. n) (const s0)
   DV.fromList `fmap` unfoldrNM iter m initvec
  where
    coef1 = dt*(r-0.5*vol*vol)
    coef2 = vol*sqrt(dt)

    iter :: Array U DIM1 Double -> IO (Array U DIM1 Double)
    iter prev = do
      ran <- rng n
      computeP $ zipWith (\s x -> s * exp(coef1 + x*coef2)) prev ran

iv :: Monad m => Array U DIM1 Double -> m (Array U DIM1 Double)
iv arr = computeP $ map (\x -> max (k-x) 0) arr

average :: Source r Double => Array r DIM1 Double -> Double
average xs = sumAllS xs / (fromIntegral $ length xs)

zip :: (Shape sh, Source r1 a, Source r2 b) => Array r1 sh a -> Array r2 sh b -> Array D sh (a,b)
zip = zipWith (,)

pick :: (Unbox a, Monad m, Source r a) => Array D DIM1 Bool -> Array r DIM1 a -> m (Array U DIM1 a)
pick p xs = selectP (index p . index1) (index xs . index1) (length xs)

lsm :: Int -> Int -> IO Double
lsm n_points n_paths = do
  s <- genPaths n_points n_paths
  init_disccashflow <- iv (DV.last s)
  res <- DV.foldM lsm' init_disccashflow (DV.reverse $ DV.init s)
  return . average $ res
 where 
  exercise_decision :: Double -> (Double, Double) -> Double
  exercise_decision ev (iv, v) | iv > 0 && iv > ev = iv
                               | otherwise = v

  lsm' :: Array U DIM1 Double -> Array U DIM1 Double -> IO (Array U DIM1 Double)
  lsm' disccashflow s = computeP =<< do
    intrinsic_value <- iv s
    let p = map (>0) intrinsic_value
        default_out = map (*df) disccashflow
    y <- pick p default_out
    spick <- pick p s
    rg <- polyfit (delay spick) (delay y) reg
    let estimatedtimevalue = polyvals rg (delay s)
    return (zipWith exercise_decision 
            estimatedtimevalue
            (zip intrinsic_value default_out) :: Array D DIM1 Double)

main :: IO ()
main = print =<< lsm n_points n_paths

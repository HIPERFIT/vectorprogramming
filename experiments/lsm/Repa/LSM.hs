module LSM where

import Data.List (intersperse)
import System.Random
import Data.Random.Normal
import Control.Monad
import Control.DeepSeq
import LinAlg

import Prelude hiding ((++), reverse, take, map, zipWith, length, zip)
import qualified Prelude

import qualified Data.Vector as DV
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Array.Repa
import RepaHelpers

-- Simulation parameters
n_paths :: Int
n_paths = 20000          -- time steps
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

-- ^ Generate @n@ samples from a mean=0, stddev=1 normal distribution
rng :: Int -> IO (Array D DIM1 Double)
rng n = do
  gen <- newStdGen
  let m = n `div` 2
  let list = fromListUnboxed (Z :. m) . Prelude.take m $ normals gen
  return $ list ++ (reverse $ map (* (-1)) list) -- TODO: is this reverse necessary?

-- unfoldrNMRepa :: (Monad m) => (Array D DIM1 Double -> m (Array D DIM1 Double))
--                  -> Int -> Array D DIM1 Double -> m (Array D DIM2 Double)
-- unfoldrNMRepa f n seed = do
--   arrs <- unfoldrNM f n seed
--   let arrs' = Prelude.map (\arr -> reshape (Z :. (length arr) :. 1) arr) arrs
--   return $ Prelude.foldl1 (++) arrs'

-- ^ Monadic unfoldr. Stops unfolding after N steps and includes the
-- seed in the result.
unfoldrNM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
unfoldrNM _ 0 seed = return [seed]
unfoldrNM f n seed = do
  x <- f seed
  xs <- unfoldrNM f (n-1) x
  return (seed : xs)

-- testUnfold :: IO [Int]
-- testUnfold = unfoldrNM (\x -> return $ x + 1) 5 0

-- ^ Generate paths
genPaths :: Int -> Int -> IO (DV.Vector (Array D DIM1 Double))
genPaths m n = DV.fromList `fmap` unfoldrNM iter m initvec
  where
    coef1 = dt*(r-0.5*vol*vol)
    coef2 = vol*sqrt(dt)
    initvec :: Array D DIM1 Double
    initvec = fromFunction (Z :. n) (const s0)

    iter :: Array D DIM1 Double -> IO (Array D DIM1 Double)
    iter prev = do
      ran <- rng n
      return $ zipWith (\s x -> s * exp(coef1 + x*coef2)) prev ran

iv :: Array D DIM1 Double -> Array D DIM1 Double
iv = map (\x -> max (k-x) 0)

average :: Array D DIM1 Double -> Double
average xs = sumAllS xs / (fromIntegral $ length xs)

zip :: (Shape sh, Source r1 a, Source r2 b) => Array r1 sh a -> Array r2 sh b -> Array D sh (a,b)
zip = zipWith (,)

pick :: (Unbox a, Monad m) => Array D DIM1 Bool -> Array D DIM1 a -> m (Array U DIM1 a)
pick p xs = selectP (index p . index1) (index xs . index1) (length xs)

lsm :: Int -> Int -> IO Double
lsm n_points n_paths = do
  s <- genPaths n_points n_paths
  let init_disccashflow = iv (DV.last s) :: Array D DIM1 Double
  res <- DV.foldM lsm' init_disccashflow (DV.reverse $ DV.init s)
  return . average $ res
 where 
  exercise_decision :: Double -> (Double, Double) -> Double
  exercise_decision ev (iv, v) | iv > 0 && iv > ev = iv
                               | otherwise = v

  lsm' :: Array D DIM1 Double -> Array D DIM1 Double -> IO (Array D DIM1 Double)
  lsm' disccashflow s = do
    let intrinsic_value :: Array D DIM1 Double
        intrinsic_value = iv s
        p = map (>0) intrinsic_value
        default_out = map (*df) disccashflow
    y <- pick p default_out
    spick <- pick p s
    rg <- polyfit (delay spick) (delay y) reg
    let estimatedtimevalue = polyvals rg s
    return $ zipWith exercise_decision 
                estimatedtimevalue
                (zip intrinsic_value default_out)

main :: IO ()
main = print =<< lsm n_points n_paths
 where
   n_points = 252 -- time steps
   n_paths = 2000

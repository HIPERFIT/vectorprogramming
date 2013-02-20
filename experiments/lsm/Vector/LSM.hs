import qualified Data.Vector.Unboxed as UB
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector as B
import Data.List (intersperse)

import Control.Monad
import Control.DeepSeq

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


rng gen count =
  let normalsvec = normals gen (count `div` 2)
  in normalsvec UB.++ (UB.reverse $ UB.map (* (-1)) normalsvec)

-- ^ Generate paths
genPaths :: PureMT -> Int -> Int -> IO (B.Vector (Vector Double))
genPaths gen m n = do 
  return $ B.unfoldrN m iter (initvec, normalsvec)
  where
    coef1 = dt*(r-0.5*vol*vol)
    coef2 = vol*sqrt(dt)
    initvec :: Vector Double
    initvec = UB.generate n (const s0)

    normalsvec = rng gen (m*n)

    iter :: (Vector Double, Vector Double) -> Maybe (Vector Double, (Vector Double, Vector Double))
    iter (prev, seq) =
      let (ran, rest) = UB.splitAt n seq
          result = UB.zipWith (\s x -> s * exp(coef1 + x*coef2)) prev ran
      in Just (result, (result,rest))

iv :: Vector Double -> Vector Double
iv = UB.map (\x -> max (k-x) 0)

average :: Vector Double -> Double
average xs = UB.sum xs / (fromIntegral $ UB.length xs)

variance :: Vector Double -> Double
variance xs = average (UB.map (\x -> (avg-x)^2) xs)
  where avg = average xs

std :: Vector Double -> Double
std = sqrt . variance

pick :: UB.Unbox a => Vector (Bool, a) -> Vector a
pick = UB.map snd . UB.filter fst

lsm :: Int -> Int -> IO (Vector Double)
lsm  n_points n_paths = do
  gen <- newPureMT
  s <- genPaths gen n_points n_paths
  let init_disccashflow = iv (B.last s) :: Vector Double
  B.foldM lsm' init_disccashflow (B.reverse $ B.init (B.tail s))
 where 
  exercise_decision ev iv v | iv > 0 && iv > ev = iv
                            | otherwise = v

  lsm' :: Vector Double -> Vector Double -> IO (Vector Double)
  lsm' disccashflow s = do
    let intrinsic_value = iv s
        p = UB.map (>0) intrinsic_value
        default_out = UB.map (*df) disccashflow
        y = pick $ UB.zip p default_out
        spick = pick $ UB.zip p s
        rg = polyfit spick y reg
        estimatedtimevalue = polyvals rg s
    return $ UB.zipWith3 exercise_decision 
                estimatedtimevalue
                intrinsic_value
                default_out

main :: IO ()
main = do
  disccashflow <- lsm n_points n_paths
  let v0 = df * average disccashflow
  let v0' = if k-s0 > v0 then k-s0 else v0
  print v0'
  print $ 1.96 * std(UB.map (df*) disccashflow)/sqrt(fromIntegral $ UB.length disccashflow)


-- We should obtain these values from paths1.csv
-- 7.62729165745
-- 1.55644862876

-- readPaths :: IO (B.Vector (Vector Double))
-- readPaths = do
--   content <- readFile "paths1.csv"
--   let processline :: String -> Vector Double
--       processline line = UB.fromList . map read $ wordsWhen (==',') line
--   return . B.fromList . map processline $ lines content

-- wordsWhen     :: (Char -> Bool) -> String -> [String]
-- wordsWhen p s =  case dropWhile p s of
--                       "" -> []
--                       s' -> w : wordsWhen p s''
--                             where (w, s'') = break p s'

-- writePaths :: Vector (Vector Double) -> IO ()
-- writePaths xs = zipWithM_ printPath [1..] (UB.toList $ transpose xs)
--   where
--     printPath :: Int -> Vector Double -> IO ()
--     printPath i xs = zipWithM_ (\x -> putStrLn . ((show i ++ "," ++ show x ++ ",") ++) . show) [0..] $ UB.toList xs

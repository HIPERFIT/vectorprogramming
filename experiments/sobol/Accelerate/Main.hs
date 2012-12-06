module Main where

import Sobol
import BenchmarkRunner.Main(runTest)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList, fromList, index1)
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart


runsobol :: Int -> [Double]
runsobol = toList . run . sobolInd . constant

sobolSequence :: Int -> [[Double]]
sobolSequence n = Prelude.map runsobol [0..n-1]

runsobol' :: String
runsobol' = show . run $ sobolIndices

-- main = do 
--   i <- read `fmap` getLine
--   putStrLn $ "Running sobol. Input: " ++ show i
--   putStrLn "Output:"
--   print $ sobolSequence i

sobSeq :: Int -> Array DIM2 SpecReal
sobSeq n = run $ mapsobolInd (fromList (Z :. n) [0..n])

main = do
  i <- read `fmap` getLine
  putStrLn $ "Running sobol. Input: " ++ show i
  putStrLn "Output:"
  print $ sobSeq i

--main = runTest sobolSequence

module Main where

import Sobol
import BenchmarkRunner.Main(runTest)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList)


runsobol :: Int -> [Double]
runsobol = toList . run . sobolInd . constant

sobolSequence :: Int -> [[Double]]
sobolSequence n = Prelude.map runsobol [0..n-1]

-- runsobol' :: String
-- runsobol' = show . run $ sobolIndices


-- main = do 
--   i <- read `fmap` getLine
--   putStrLn $ "Running sobol. Input: " ++ show i
--   putStrLn "Output:"
--   print $ sobolSequence i

main = runTest sobolSequence

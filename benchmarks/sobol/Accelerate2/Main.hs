module Main where

import Sobol
import BenchmarkRunner.Main(runTest)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList, fromList, index1, arrayShape)
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart

sobSeq :: Int -> Array DIM2 SpecReal
sobSeq n = run $ mapsobolInd (fromList (Z :. n) [0..n])

to2DList :: Elt a => Array DIM2 a -> [[a]]
to2DList arr =
  let Z :. n :. m = arrayShape arr
  in tile m $ toList arr

tile :: Int -> [a] -> [[a]]
tile n xs | Prelude.length xs >= n = take n xs : (tile n $ drop n xs)
          | otherwise = [xs]

sobolSequence :: Int -> [[Double]]
sobolSequence = to2DList . sobSeq

-- main = do
--   i <- read `fmap` getLine
--   putStrLn $ "Running sobol. Input: " ++ show i
--   putStrLn "Output:"
--   print $ sobSeq i

main = runTest sobolSequence

module Main where

import American
import BinomialBenchmark.Main(runTest)

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter (run)
--import Data.Array.Accelerate.CUDA (run)

main = runTest go
  where
    go :: Int -> FloatRep
    go n = head $ A.toList $ run (binom n)
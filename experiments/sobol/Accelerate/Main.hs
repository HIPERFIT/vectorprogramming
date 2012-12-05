module Main where

import Sobol
import BenchmarkRunner.Main(runTest)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList)

main = runTest (\n -> toList . run . sobolInd $ constant n)

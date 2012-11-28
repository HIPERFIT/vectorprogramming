module Main where

import American
import BenchmarkRunner.Main(runTest)

main = kernels `seq` runTest (binomRun kernels)
  where kernels = binom defaultModel
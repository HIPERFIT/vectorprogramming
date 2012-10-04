module Main where

import American
import BinomialBenchmark.Main(runTest)

main = kernels `seq` runTest (binomRun kernels)
  where kernels = binomCompiled defaultModel

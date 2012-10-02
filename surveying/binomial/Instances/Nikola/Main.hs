module Main where

import American
import BinomialTest (runTest)

main = kernels `seq` runTest (binomRun kernels)
  where kernels = binom defaultModel
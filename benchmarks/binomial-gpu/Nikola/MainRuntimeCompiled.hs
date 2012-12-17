module Main where

import qualified Data.Array.Nikola.Backend.CUDA as C
import American
import BenchmarkRunner.Main(runTest, cfgModSummaryFile)

main = do
  C.initializeCUDACtx
  kernelsUnComp `seq` runTest (binomRun kernelsUnComp)
  where kernelsUnComp = binom defaultModel

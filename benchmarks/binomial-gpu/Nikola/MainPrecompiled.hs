module Main where

import qualified Data.Array.Nikola.Backend.CUDA as C
import American
import BenchmarkRunner.Main(runTest, cfgModSummaryFile)

main = do
  C.initializeCUDACtx
  kernelsComp `seq` runTest (binomRun kernelsComp)
  where kernelsComp = binomCompiled defaultModel

module Main where

import qualified Data.Array.Nikola.Backend.CUDA as C
import American
import BenchmarkRunner.Main(runTestWith, cfgModSummaryFile)

main = do
  C.initializeCUDACtx
  kernelsUnComp `seq` runTestWith (cfgModSummaryFile (++ "-Uncompiled")) (binomRun kernelsUnComp)
  kernelsComp `seq` runTestWith (cfgModSummaryFile (++ "-Precompiled")) (binomRun kernelsComp)
  where kernelsUnComp = binom defaultModel
        kernelsComp = binomCompiled defaultModel

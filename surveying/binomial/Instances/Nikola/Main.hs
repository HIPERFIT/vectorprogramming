module Main where

import American
import BinomialBenchmark.Main(runTestWith, cfgModSummaryFile)

main = do
  kernelsUnComp `seq` runTestWith (cfgModSummaryFile (++ "-Uncompiled")) (binomRun kernelsUnComp)
  kernelsComp `seq` runTestWith (cfgModSummaryFile (++ "-Precompiled")) (binomRun kernelsComp)
  where kernelsUnComp = binom defaultModel
        kernelsComp = binomCompiled defaultModel

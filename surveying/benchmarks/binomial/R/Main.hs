module Main where

import BenchmarkRunner.External(initialiseExt, benchmarkExt, terminateExt)
import BenchmarkRunner.Main(runTestIO)

main = do
                                              -- Definitely not cosher path
  proc <- initialiseExt "Rscript" ["--vanilla", "benchmarks/binomial/R/AmericanPut.R"]
  -- for debugging:
  -- proc <- initialiseExt "nc" ["localhost","2000"]
  runTestIO $ benchmarkExt proc
  terminateExt proc

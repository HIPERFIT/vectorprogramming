module Main where

import BinomialBenchmark.External(initialiseExt, benchmarkExt, terminateExt)
import BinomialBenchmark.Main(runTestIO)

main = do
                                              -- Definitely not cosher path
  proc <- initialiseExt "Rscript" ["--vanilla", "Instances/R/AmericanPut.R"]
  -- for debugging:
  -- proc <- initialiseExt "nc" ["localhost","2000"]
  runTestIO $ benchmarkExt proc
  terminateExt proc

module Main where

import BenchmarkRunner.External(initialiseExt, benchmarkExt, terminateExt)
import BenchmarkRunner.Main(runTestIO)
import System.Process
import System.Exit

main = do
  -- ** Deprecated - now done by run.sh **
  -- compile source code.
  -- Definitely not cosher paths
  -- exitcode <- runCommand "cd ../../benchmarks/binomial-cpu/C/;make" >>= waitForProcess

  runCommand "pwd" >>= waitForProcess
  proc <- initialiseExt "./AmrPut" []
  runTestIO $ benchmarkExt proc
  terminateExt proc

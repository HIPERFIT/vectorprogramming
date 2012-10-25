module Main where

import BenchmarkRunner.External(initialiseExt, benchmarkExt, terminateExt)
import BenchmarkRunner.Main(runTestIO)
import System.Process
import System.Exit

main = do
  -- compile source code.
  -- Definitely not cosher paths
  exitcode <- runCommand "cd ./benchmarks/binomial/Cuda/src-cpp/;make" >>= waitForProcess
  if exitcode == ExitSuccess then do
    proc <- initialiseExt "./benchmarks/binomial/Cuda/src-cpp/bin/binomialOptions" []
    runTestIO $ benchmarkExt proc
    terminateExt proc
    else do
    putStrLn $ "make exited with " ++ show exitcode
    exitWith exitcode

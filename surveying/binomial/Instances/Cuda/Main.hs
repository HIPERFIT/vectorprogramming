module Main where

import BinomialBenchmark.External(initialiseExt, benchmarkExt, terminateExt)
import BinomialBenchmark.Main(runTestIO)
import System.Process
import System.Exit

main = do
  -- compile source code.
  -- Definitely not cosher paths
  exitcode <- runCommand "cd src-cpp/;make" >>= waitForProcess
  if exitcode == ExitSuccess then do
    proc <- initialiseExt "./Instances/Cuda/scr-cpp/bin/binomialOptions" []
    runTestIO $ benchmarkExt proc
    terminateExt proc
    else do
    putStrLn $ "make exited with " ++ show exitcode
    exitWith exitcode

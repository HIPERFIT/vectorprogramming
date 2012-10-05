module Main where

import BinomialBenchmark.External(initialiseExt, benchmarkExt, terminateExt)
import BinomialBenchmark.Main(runTestIO)
import System.Process
import System.Exit

main = do
  -- compile source code.
  -- Definitely not cosher paths
  exitcode <- runCommand "cd ./Instances/Cuda/src-cpp/;make" >>= waitForProcess
  if exitcode == ExitSuccess then do
    putStrLn "initializing.."
    proc <- initialiseExt "./Instances/Cuda/src-cpp/bin/binomialOptions" []
    putStrLn "initialized!"
    runTestIO $ benchmarkExt proc
    terminateExt proc
    else do
    putStrLn $ "make exited with " ++ show exitcode
    exitWith exitcode

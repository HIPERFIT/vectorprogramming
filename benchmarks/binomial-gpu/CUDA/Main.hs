module Main where

import BenchmarkRunner.External(initialiseExt, benchmarkExt, terminateExt)
import BenchmarkRunner.Main(runTestIO)
import System.Process
import System.Exit

import System.SetEnv

main = do
  setEnv "LD_LIBRARY_PATH" "/usr/local/cuda-5.0/lib64"
  setEnv "PATH" "/bin:/usr/bin:/usr/local/cuda-5.0/bin"
  -- compile source code.
  -- Definitely not cosher paths
  exitcode <- runCommand ("cd ../../benchmarks/binomial-gpu/CUDA/src-cpp/;"
                         ++" make")
                >>= waitForProcess
  if exitcode == ExitSuccess then do
    proc <- initialiseExt ("../../benchmarks/binomial-gpu/CUDA/src-cpp/bin/binomialOptions") []
    runTestIO $ benchmarkExt proc
    terminateExt proc
    else do
    putStrLn $ "make exited with " ++ show exitcode
    exitWith exitcode

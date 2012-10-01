module BinomialBenchmark.External(
  initialiseExt,
  benchmarkExt,
  terminateExt,
  ExternalProcess(..)) where

-- This module allows for the benchmarking of external processes
-- The test protocol is very simplistic:
--
-- Benchmark :=
--   (write :) [0-9]+  // the test to run
--   (read  :) "OK"    // blocking read, done when finished.
--
-- Exit :=
--   (write :) "EXIT"
--   (read  :) "OK"
--   (terminate self)
--
-- Global :=
--   (read : ) "OK"
--   Benchmark*
--   Exit

import Criterion

import System.Process
import System.IO

data ExternalProcess = ExternalProcess{
  epIn :: Handle, epOut :: Handle, epProc :: ProcessHandle
  }

-- | start an external process, wait for "OK"
initialiseExt cmd args = do
  (Just hIn, Just hOut, _, ph) <- createProcess
    CreateProcess{
      cmdspec = RawCommand cmd args,
      cwd = Nothing, env = Nothing,
      std_in = CreatePipe, std_out = CreatePipe,
      std_err = Inherit, close_fds = True,
      create_group = False}
  res <- hGetLine hOut
  if res == "OK" then
      return $ ExternalProcess hIn hOut ph
    else
      error $ cmd ++ show args ++ " outputted " ++ res ++ ", expected OK."

benchmarkExt (ExternalProcess hIn hOut _) benchArg = do
  hPutStrLn hOut $ show benchArg
  res <- hGetLine hIn
  case res of
    'O':'K':_ -> return ()
    _ -> error $ "external process outputted " ++ res ++ ", expected OK."

terminateExt (ExternalProcess hIn hOut ph) = do
  hPutStrLn hOut $ "EXIT"
  res <- hGetLine hIn
  case res of
    'O':'K':_ -> return ()
    _ -> error $ "external process outputted " ++ res ++ ", expected OK."
  hClose hIn -- probably unnecessary, but here anyway.
  hClose hOut
  waitForProcess ph

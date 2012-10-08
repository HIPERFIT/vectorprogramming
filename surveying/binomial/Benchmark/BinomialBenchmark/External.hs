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
--   (read  :) "RESULT"    // blocking read, done when finished.
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

import System.Exit
import System.Process
import System.IO

data ExternalProcess = ExternalProcess{
  epIn :: Handle, epOut :: Handle, epProc :: ProcessHandle
  }

-- | start an external process, wait for "OK"
initialiseExt cmd args = do
  -- stuff to debug our current directory
  -- readProcess "pwd" [] "" >>= putStrLn
  -- readProcess "ls" [] "" >>= putStrLn
  (Just hIn, Just hOut, _, ph) <- createProcess
    CreateProcess{
      cmdspec = RawCommand cmd args,
      cwd = Nothing, env = Nothing,
      std_in = CreatePipe, std_out = CreatePipe,
      std_err = Inherit, close_fds = True,
      create_group = False}
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  maybeExited <- getProcessExitCode ph
  case maybeExited of
    Just exitcode -> do
      putStrLn ("command " ++ cmd ++ " exited with " ++ show exitcode)
      exitWith exitcode
    Nothing -> do
       res <- hGetLine hOut
       if res == "OK" then
           return $ ExternalProcess hIn hOut ph
         else
           error $ cmd ++ show args ++ " outputted " ++ res ++ ", expected OK."

benchmarkExt (ExternalProcess hIn hOut _) benchArg = do
  hPutStrLn hIn $ show benchArg
  -- hFlush hIn
  res <- hGetLine hOut
  case res of
    'R':'E':'S':'U':'L':'T':_ -> return ()
    _ -> error $ "external process outputted " ++ res ++ ", expected RESULT."

terminateExt (ExternalProcess hIn hOut ph) = do
  hPutStrLn hIn $ "EXIT"
  -- hFlush hIn
  res <- hGetLine hOut
  case res of
    'O':'K':_ -> return ()
    _ -> error $ "external process outputted " ++ res ++ ", expected OK."
  hClose hIn -- probably unnecessary, but here anyway.
  hClose hOut
  waitForProcess ph

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Nikola (binom)
import Options

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initializeCUDACtx
  putStrLn "OK"
  execute (binom sampleOpt)


execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

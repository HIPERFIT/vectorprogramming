module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import qualified American.Nikola as AMN

import System.IO

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initializeCUDACtx
  putStrLn "OK"
  execute AMN.binom


execute :: (Read a) => (a -> IO b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  r <- f (read str)
  putStrLn $ "RESULT " -- ++ (take 150 . show . f . read $ str)

{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import System.IO

import LSM

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute (lsm n_points)

execute :: (Read a, Show b) => (a -> IO b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  result <- f . read $ str
  putStrLn $ "RESULT " Prelude.++ (show result)


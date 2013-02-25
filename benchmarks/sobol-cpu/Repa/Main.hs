{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import System.IO

import Sobol

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute sobolSequence_

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " Prelude.++ (take 150 . show . f . read $ str)


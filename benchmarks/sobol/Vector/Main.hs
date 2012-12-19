module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Sobol

main = do
  putStrLn "OK" -- no preparation steps
  execute sobolSequence

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)
module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, unit, toList)

import Pi(runPi)
import System.IO

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  -- Wrap it properly:
  execute $ head . toList . computepi

computepi = run . runPi . unit . constant

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (show . f . read $ str)




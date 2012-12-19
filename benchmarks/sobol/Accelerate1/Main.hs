module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList, fromList, index1)
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart

import Sobol

runsobol :: Int -> [Double]
runsobol = toList . run . sobolInd . constant

sobolSequence :: Int -> [[Double]]
sobolSequence n = Prelude.map runsobol [0..n-1]

main = do
  putStrLn "OK" -- no preparation steps
  execute sobolSequence

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

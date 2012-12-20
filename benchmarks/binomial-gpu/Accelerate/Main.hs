module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as A

import American

binom :: Int -> FloatRep
binom = head . A.toList . A.run . binomAcc

main = do
  putStrLn "OK" -- no preparation steps
  execute binom

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)


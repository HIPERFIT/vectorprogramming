module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Accelerate.CUDA (run1)
import Data.Array.Accelerate (indexArray, fromList, Scalar, Z(..), Elt, Acc)

import Pi(runPi)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute computepi

computepi :: Int -> Float
computepi = runScalar1 runPi

runScalar1 :: (Elt a, Elt b) => (Acc (Scalar a) -> Acc (Scalar b)) -> (a -> b)
runScalar1 f x = (run1 f $ fromList Z [x]) `indexArray` Z

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (show . f . read $ str)




module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as A
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import American
import System.IO

fromScalar :: Sugar.Elt e => Sugar.Scalar e -> e
fromScalar x = x Sugar.! Sugar.Z

binom :: Int -> FloatRep
binom = fromScalar . A.run . binomAcc

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute binom

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (show . f . read $ str)


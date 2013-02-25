module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import Control.DeepSeq(($!!), NFData(..))

import qualified Data.Vector.Storable as V
import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList, fromList, index1, arrayShape, generate)
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.IO

import Sobol
import System.IO

sobSeq :: Int -> Scalar SpecReal
sobSeq = run . mapsobolInd . constant

sobolSequence :: Int -> V.Vector Double
sobolSequence = snd . toVectors . sobSeq

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute sobolSequence

execute :: (NFData b, Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show $!! f . read $ str)


module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate (constant, toList, fromList, index1, arrayShape)
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Smart

import Sobol
import System.IO

sobSeq :: Int -> Array DIM2 SpecReal
sobSeq n = run $ mapsobolInd (fromList (Z :. n) [0..n])

to2DList :: Elt a => Array DIM2 a -> [[a]]
to2DList arr =
  let Z :. n :. m = arrayShape arr
  in tile m $ toList arr

tile :: Int -> [a] -> [[a]]
tile n xs | Prelude.length xs >= n = take n xs : (tile n $ drop n xs)
          | otherwise = [xs]

sobolSequence :: Int -> [[Double]]
sobolSequence = to2DList . sobSeq

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute sobolSequence

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

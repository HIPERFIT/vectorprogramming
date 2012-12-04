module Main where

import qualified Data.Array.Nikola.Backend.CUDA as N
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import Data.Array.Nikola.Backend.CUDA
import Data.Int

example1 :: Exp Int32 -> Exp Int32
example1 n = foldl (+) 0 [1..n]

example2 :: Int -> Exp Int32
example2 n = foldl (\x y -> x + (fromIntegral y)) 0 [1..n]

-- example1Compiled :: Int -> Float
-- example1Compiled n = NH.compile (example1 n)

example2Compiled :: Int -> Int32
example2Compiled n = NH.compile (example2 n)


main = do
  N.initializeCUDACtx
--  print $ example1Compiled 10
  print $ example2Compiled 10

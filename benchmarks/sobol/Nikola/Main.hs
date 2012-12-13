module Main where

import Sobol
--import BenchmarkRunner.Main(runTest)

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Data.Int

sobolSequence :: Int32 -> [[SpecReal]]
sobolSequence n = map (CV.toList . sobol) [0..n-1]
  where
    sobol :: Int32 -> CV.Vector SpecReal
    sobol = NH.compile sobolInd

main = do
  initializeCUDACtx
  print $ sobolSequence 20

--main = runTest sobolSequence

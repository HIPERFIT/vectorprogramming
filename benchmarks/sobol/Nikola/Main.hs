{-# LANGUAGE TemplateHaskell #-}

module Main where

import Sobol
import BenchmarkRunner.Main

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Data.Int

sobolIndRuntimeCompiled :: Int -> CV.Vector SpecReal
sobolIndRuntimeCompiled = NH.compile sobolInd . Prelude.fromIntegral

sobolIndPrecompiled :: Int32 -> CV.Vector SpecReal
sobolIndPrecompiled = $(NTH.compileSig sobolInd (undefined :: Int32 -> CV.Vector SpecReal))

sobolSequence :: (Int -> CV.Vector SpecReal) -> Int -> [[SpecReal]]
sobolSequence sobol n = map (CV.toList . sobol) [0..n-1]

main = do
  initializeCUDACtx
  sobolIndRuntimeCompiled `seq` runTestWith (cfgModSummaryFile (++ "-Uncompiled")) (sobolSequence sobolIndRuntimeCompiled)
  sobolIndPrecompiled `seq` runTestWith (cfgModSummaryFile (++ "-Precompiled")) (sobolSequence (sobolIndPrecompiled . Prelude.fromIntegral))
  -- runTest sobolSequence
  -- runTest sobolSequenceCompiled

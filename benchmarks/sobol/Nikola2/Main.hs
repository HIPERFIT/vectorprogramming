{-# LANGUAGE TemplateHaskell #-}

module Main where

import Sobol
import BenchmarkRunner.Main

import qualified Data.Vector.CUDA.Storable as CV
import Data.Array.Nikola.Backend.CUDA hiding (map, (++))
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Data.Int

-- Would be nice to make the length parameter unnecessary here. We
-- haven't found away around that yet.
sobolIndRuntimeCompiled :: Int32 -> CV.Vector Int32 -> CV.Vector SpecReal
sobolIndRuntimeCompiled = NH.compile $ mapsobolIndr


sobolIndPrecompiled :: Int32 -> CV.Vector Int32 -> CV.Vector SpecReal
sobolIndPrecompiled = $(NTH.compileSig mapsobolIndr (undefined :: Int32 -> CV.Vector Int32 -> CV.Vector SpecReal))

sobolSequence :: (Int32 -> CV.Vector Int32 -> CV.Vector SpecReal) -> Int -> [SpecReal]
sobolSequence sobol n = CV.toList . sobol (Prelude.fromIntegral n) . CV.fromList $ map Prelude.fromIntegral [0..n-1]

main = do
  initializeCUDACtx
  sobolIndRuntimeCompiled `seq` runTestWith (cfgModSummaryFile (++ "-Uncompiled")) (sobolSequence sobolIndRuntimeCompiled)
  sobolIndPrecompiled `seq` runTestWith (cfgModSummaryFile (++ "-Precompiled")) (sobolSequence sobolIndPrecompiled)
  -- runTest sobolSequence
  -- runTest sobolSequenceCompiled

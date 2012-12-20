{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Int
import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)

import Sobol
import System.IO

-- sobolIndRuntimeCompiled :: Int -> CV.Vector SpecReal
-- sobolIndRuntimeCompiled = NH.compile sobolInd . Prelude.fromIntegral

sobolIndPrecompiled :: Int32 -> CV.Vector SpecReal
sobolIndPrecompiled = $(NTH.compileSig sobolInd (undefined :: Int32 -> CV.Vector SpecReal))

sobolSequence :: (Int -> CV.Vector SpecReal) -> Int -> [[SpecReal]]
sobolSequence sobol n = map (CV.toList . sobol) [0..n-1]

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initializeCUDACtx
  putStrLn "OK"
  execute (sobolSequence (sobolIndPrecompiled . Prelude.fromIntegral))

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

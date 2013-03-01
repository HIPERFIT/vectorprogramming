{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Int
import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import Control.DeepSeq(($!!), NFData(..))

import qualified Data.Vector.Storable as V
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

sobolSequence :: (Int -> CV.Vector SpecReal) -> Int -> V.Vector (CV.Vector SpecReal)
sobolSequence sobol n = V.take 150 $!! V.map (toHostVector . sobol) (V.generate n id)

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initializeCUDACtx
  putStrLn "OK"
  execute (sobolSequence (sobolIndPrecompiled . Prelude.fromIntegral))

execute :: (NFData b, Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (show . f . read $ str)

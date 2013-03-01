{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import Control.DeepSeq(($!!), deepseq, NFData(..))

import qualified Data.Vector.Storable as V
import qualified Data.Vector.CUDA.Storable as CV
import Data.Array.Nikola.Backend.CUDA hiding (map, (++),take)
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Data.Int

import Sobol
import System.IO

sobolIndPrecompiled :: Int32 -> CV.Vector SpecReal
sobolIndPrecompiled = $(NTH.compileSig mapsobolIndr (undefined :: Int32 -> CV.Vector SpecReal))

sobolSequence :: (Int32 -> CV.Vector SpecReal) -> Int -> V.Vector SpecReal
sobolSequence sobol n = V.take 150 $!! CV.toHostVector $ sobol (Prelude.fromIntegral n)

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
  putStrLn $ "RESULT " ++ (show . f . read $ str)


instance (V.Storable a, NFData a) => NFData (V.Vector a) where
  rnf v = V.foldl' (\x y -> y `deepseq` x) () v
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, forever)
import Control.DeepSeq(($!!), deepseq, NFData(..))
import System.Exit (exitSuccess)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.CUDA.Storable as CV
import Data.Array.Nikola.Backend.CUDA hiding (map, (++),take)
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Data.Int

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R

import Sobol
import System.IO

sobolIndPrecompiled :: Int32 -> IO (R.Array R.CUF R.DIM1 SpecReal)
sobolIndPrecompiled = return . $(NTH.compileSig sobolSequence_
  (undefined :: Int32 -> R.Array R.CUF R.DIM1 SpecReal))

{-
sobolSeqNoDtoH :: (Int32 -> IO (R.Array r sh a)) -> Int32 -> IO ()
sobolSeqNoDtoH sobol n =
-}

{-
sobolSequence :: (NFData a, CV.Storable a) => (Int32 -> CV.Vector a) -> Int -> V.Vector a
sobolSequence sobol n = V.take 150 $!! CV.toHostVector $ sobol (Prelude.fromIntegral n)
-}
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initializeCUDACtx
  putStrLn "OK"
  execute (sobolIndPrecompiled . Prelude.fromIntegral)

execute :: (Read a, Show b) => (a -> IO b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  vec <- f $ read str
  -- putStrLn $ "RESULT " ++ show (vec)
  putStrLn $ "RESULT <untransferred>" -- ++ (show . f . read $ str)

{-
instance (V.Storable a, NFData a) => NFData (V.Vector a) where
  rnf v = V.foldl' (\x y -> y `deepseq` x) () v
  -}

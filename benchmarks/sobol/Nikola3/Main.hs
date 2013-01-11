{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

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
sobolIndPrecompiled = $(NTH.compileSig sobolSequence_ (undefined :: Int32 -> CV.Vector SpecReal))

sobolSequence :: CV.Storable a => (Int32 -> CV.Vector a) -> Int -> V.Vector a
sobolSequence sobol n = V.take 150 $ CV.toHostVector $ sobol (Prelude.fromIntegral n)

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

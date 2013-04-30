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

import Pi
import System.IO
import Data.Word

import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R
import qualified Data.Array.Repa as R

type RDirV = R.Array R.CUF R.DIM2 Word32 

piPrecompiled :: RDirV -> Int32 -> Double
piPrecompiled = $(NTH.compileSig (\dir n -> pi2d $ sobolND dir n) (undefined :: RDirV -> Int32 -> Double))
--piPrecompiled = NH.compile (\dir n -> pi2d $ sobolND dir n)

piRun n = piPrecompiled sobol_dirVs n

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  initializeCUDACtx
  putStrLn "OK"
  execute piRun

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (show . f . read $ str)

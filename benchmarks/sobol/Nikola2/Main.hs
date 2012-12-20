{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import qualified Data.Vector.CUDA.Storable as CV
import Data.Array.Nikola.Backend.CUDA hiding (map, (++),take)
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import Data.Int

import Sobol
import System.IO

-- -- Would be nice to make the length parameter unnecessary here. We
-- -- haven't found away around that yet.
-- sobolIndRuntimeCompiled :: Int32 -> CV.Vector Int32 -> CV.Vector SpecReal
-- sobolIndRuntimeCompiled = NH.compile $ mapsobolIndr


sobolIndPrecompiled :: Int32 -> CV.Vector Int32 -> CV.Vector SpecReal
sobolIndPrecompiled = $(NTH.compileSig mapsobolIndr (undefined :: Int32 -> CV.Vector Int32 -> CV.Vector SpecReal))

sobolSequence :: (Int32 -> CV.Vector Int32 -> CV.Vector SpecReal) -> Int -> [SpecReal]
sobolSequence sobol n = CV.toList . sobol (Prelude.fromIntegral n) . CV.fromList $ map Prelude.fromIntegral [0..n-1]

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

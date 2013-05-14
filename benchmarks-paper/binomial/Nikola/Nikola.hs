{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Nikola (
    binom
  ) where

import Data.Array.Nikola.Backend.CUDA.TH (compile)
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.UnboxedForeign
import Data.Array.Repa.Repr.CUDA.UnboxedForeign
import qualified Data.Vector.UnboxedForeign as VUF
import qualified Data.Vector.Storable as V
import Data.Int
import Data.List (foldl')

import qualified Implementation as Imp
import Options

type EurOption = (Bool, Float, Float, Int32, Float, Float)

toTuple :: EurOpt -> EurOption
toTuple (EurOpt{..}) = ( opttype == Call
                       , s0
                       , strike
                       , fromIntegral expiry
                       , riskless
                       , volatility
                       )

vFinal :: Int32 -> EurOption -> Array CUF DIM1 Float
vFinal numSteps (iscall, s0, strike, expiry, riskless, volatility) =
  let vFinal' :: Int32 -> Bool -> Float -> Float -> Int32 -> Float -> Float -> Array CUF DIM1 Float
      vFinal' = $(compile Imp.vFinal)
  in vFinal' numSteps iscall s0 strike expiry riskless volatility

stepBack :: Int32 -> EurOption -> Array CUF DIM1 Float -> Int32 -> Array CUF DIM1 Float
stepBack numSteps (iscall, s0, strike, expiry, riskless, volatility) vPrev i =
  let stepBack' :: Int32 -> Bool -> Float -> Float -> Int32 -> Float -> Float
                -> Array CUF DIM1 Float -> Int32 -> Array CUF DIM1 Float
      stepBack' = $(compile Imp.stepBack)
  in stepBack' numSteps iscall s0 strike expiry riskless volatility vPrev i

binom :: EurOpt -> Int32 -> Float
binom opt numSteps = first ! (Z :. 0)
 where
   opt' = (toTuple opt)
   final, first :: Array CUF DIM1 Float
   final = vFinal numSteps opt'
   first = foldl' (stepBack numSteps opt') final [0..numSteps-1]
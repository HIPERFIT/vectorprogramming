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

vFinal :: EurOption -> Array CUF DIM1 Float
vFinal (iscall, s0, strike, expiry, riskless, volatility) =
  let vFinal' :: Bool -> Float -> Float -> Int32 -> Float -> Float -> Array CUF DIM1 Float
      vFinal' = $(compile (Imp.vFinal numSteps))
  in vFinal' iscall s0 strike expiry riskless volatility

stepBack :: EurOption -> Array CUF DIM1 Float -> Int32 -> Array CUF DIM1 Float
stepBack (iscall, s0, strike, expiry, riskless, volatility) vPrev i =
  let stepBack' :: Bool -> Float -> Float -> Int32 -> Float -> Float
                -> Array CUF DIM1 Float -> Int32 -> Array CUF DIM1 Float
      stepBack' = $(compile (Imp.stepBack numSteps))
  in stepBack' iscall s0 strike expiry riskless volatility vPrev i

binom :: EurOpt -> Float
binom opt = first ! (Z :. 0)
 where
   opt' = (toTuple opt)
   final, first :: Array CUF DIM1 Float
   final = vFinal opt'
   first = foldl' (stepBack opt') final [0..numSteps-1]
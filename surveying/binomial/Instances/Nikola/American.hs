{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module American (
    Model(..),
    defaultModel,
    binom --,
--    binomCompiled
  ) where

import Data.Int
import Data.List (foldl')

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector.Storable as V

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH

import American.Model
import qualified American.Implementation as Imp
--
-- This code is adapted from Ken Friis Larsen's implementation of pricing for
-- American options available at <http://github.com/kfl/american-options>.
--

binom :: Model -> F
binom (mdl@Model{..}) = V.head (CV.toHostVector first)
  where
    uPow :: CV.Vector F
    uPow = CV.fromHostVector $ V.generate (n+1) (u^)

    dPow :: CV.Vector F
    dPow = CV.fromHostVector $ V.reverse $ V.generate (n+1) (d^)

    first :: CV.Vector F
    first = foldl' (prevPut uPow dPow) (finalPut uPow dPow)
              [fromIntegral n, fromIntegral n-1 .. 1]

    finalPut :: CV.Vector F
             -> CV.Vector F
             -> CV.Vector F
    finalPut = NH.compile $ Imp.finalPut mdl

    prevPut :: CV.Vector F
            -> CV.Vector F
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
    prevPut = NH.compile $ Imp.prevPut mdl

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)

binomCompiled :: Model -> F
binomCompiled (mdl@Model{..}) = V.head (CV.toHostVector first)
  where
    uPow :: CV.Vector F
    uPow = CV.fromHostVector $ V.generate (n+1) (u^)

    dPow :: CV.Vector F
    dPow = CV.fromHostVector $ V.reverse $ V.generate (n+1) (d^)

    first :: CV.Vector F
    first = foldl' (prevPut uPow dPow) (finalPut uPow dPow)
              [fromIntegral n, fromIntegral n-1 .. 1]

    finalPut = finalPut'' strike s0
    prevPut = prevPut'' strike s0 (fromIntegral expiry) (fromIntegral bankDays) alpha sigma r

    finalPut'' :: F
             -> F
             -> CV.Vector F
             -> CV.Vector F
             -> CV.Vector F
    finalPut'' = $(NTH.compileSig Imp.finalPut' (undefined :: F
                                                        -> F
                                                        -> CV.Vector F
                                                        -> CV.Vector F
                                                        -> CV.Vector F))
    prevPut'' :: F
            -> F
            -> Int32
            -> Int32
            -> F
            -> F
            -> F
            -> CV.Vector F
            -> CV.Vector F
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
    prevPut'' = $(NTH.compileSig Imp.prevPut' (undefined :: F
                                                       -> F
                                                       -> Int32
                                                       -> Int32
                                                       -> F
                                                       -> F
                                                       -> F
                                                       -> CV.Vector F
                                                       -> CV.Vector F
                                                       -> CV.Vector F
                                                       -> Int32
                                                       -> CV.Vector F))

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)

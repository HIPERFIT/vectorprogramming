{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol where

import System.Environment
import Prelude hiding (map, filter, zipWith, zip)
import qualified Prelude
import Data.Word as DW
import Data.Int
import Data.Array.Nikola.Backend.CUDA hiding ((^))

type Elem = DW.Word32
type SpecReal = Double
type Index = Int32

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor :: Exp SpecReal
sobol_divisor = 2^30
--sobol_dirVs = [[2^k | k <- [29,28..0]]]

sobol_dirVs_array :: Array D DIM2 (Exp Elem)
sobol_dirVs_array = fromFunction (Z :. 1 :. 30)
                                 (\(Z :. i :. j) -> fromIntegral $ 2^(29-j))


grayCode :: Exp Index -> Exp Elem
grayCode n = ((fromIntegral n) `xor` ((fromIntegral n) `shiftR` 1))


--- Nikola does not have shifts, xor, testBit or fold, for now we just
--- use functions with the "correct" types (we are still a bit unsure
--- about the type of fold)
xor = (|*)
shiftR = (|*)

testBit :: Exp DW.Word32 -> Exp Int32 -> Exp Bool
testBit _ _ = undefined

fold :: Shape ix =>
        Source r a =>
        IsArray r a =>
        IsElem a => 
     (a -> a -> a) -> a -> Array r (ix :. Exp Ix) a -> Array D ix a
fold = undefined
-------------


sobolInd_ :: Exp Index -> Array D DIM1 (Exp Elem)
sobolInd_ n =
  let 
    indices :: Array D DIM2 (Exp Index)
    indices = fromFunction (extent sobol_dirVs_array)
                           (\(Z :. i :. j) -> j)
    ps :: Array D DIM2 (Exp Bool)
    ps = map (testBit (grayCode n)) indices

    doit :: (Exp Elem, Exp Bool) -> (Exp Elem, Exp Bool) -> (Exp Elem, Exp Bool)
    doit a@(xa,ia) b@(xb,ib) =
         if ia then if ib then (lift $ xa `xor` xb, lift ia)
                    else a
         else b
              
    z :: Array D DIM2 (Exp Elem, Exp Bool)
    z = zip sobol_dirVs_array ps
    
    f :: Array D DIM1 (Exp Elem, Exp Bool)
    f = fold doit (lift (0 :: Elem), lift True) z
  in map fst f


sobolInd :: Exp Index -> Array D DIM1 (Exp SpecReal)
sobolInd n = map norm (sobolInd_ n)
    where
      norm :: Exp Elem -> Exp SpecReal
      norm = ( / sobol_divisor) . fromIntegral

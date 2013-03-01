{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol where

import System.Environment
import Prelude hiding (map, filter, zipWith, zip, iterate, fromIntegral, (^))
import qualified Prelude
import Data.Word as DW
import Data.Int
import Data.Typeable

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Combinators

type Elem = DW.Word32
type SpecReal = Double
type Index = Int32

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor :: Exp SpecReal
sobol_divisor = fromInt $ bit 30
--sobol_dirVs = [[2^k | k <- [29,28..0]]]

sobol_dirVs_array :: Array D DIM2 (Exp Elem)
sobol_dirVs_array = fromFunction (Z :. 1 :. 30)
                                 (\(Z :. i :. j) -> bit (29-(fromInt j)))

grayCode :: Exp Index -> Exp Elem
grayCode n = ((fromIntegral n) `xor` ((fromIntegral n) `shiftR` 1))

fromIntegral :: (IsIntegral CUDA a, IsNum CUDA b) => Exp a -> Exp b
fromIntegral = fromInt . toInt

bit x = 1 `shiftL` x
x `testBit` i = (x &* bit i) /=* 0

fold :: Shape ix =>
        Source r a =>
        IsArray r a =>
        IsElem a =>
        Unlift CUDA a =>
        IsElem (Exp (Lifted CUDA a)) =>
        Typeable (Lifted CUDA a) =>
     (a -> a -> a) -> a -> Array r (ix :. Exp Ix) a -> Array D ix a
fold f b arr =
  let (sh :. n, lookup) = toFunction arr
      buildArray ix = snd $ iterate n iter (0,b)
        where
          iter (i,x) = (i+1, f (lookup (ix :. i)) x)
  in fromFunction sh buildArray

sobolInd_ :: Exp Index -> Array D DIM1 (Exp Elem)
sobolInd_ n =
  let
    indices :: Array D DIM2 (Exp Index)
    indices = fromFunction (extent sobol_dirVs_array)
                           (\(Z :. i :. j) -> j)
    ps :: Array D DIM2 (Exp Bool)
    ps = map (testBit (fromIntegral $ grayCode n)) indices

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

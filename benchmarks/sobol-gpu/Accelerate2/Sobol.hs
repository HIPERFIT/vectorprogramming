{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol where

import Data.Word (Word32)
import Data.Bits hiding (shiftR, testBit)
import qualified Data.Vector as VB

import Prelude hiding (map, filter, fromIntegral, zipWith, replicate, zip, fst, snd)
import qualified Prelude

import Data.Array.Accelerate hiding (Elem)

type Elem = Word32
type SpecReal = Double
type Index = Int

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor :: Exp SpecReal
sobol_divisor = Prelude.fromIntegral (2^30)
sobol_dirVs = [ [2^k | k <- [29,28..0]]]

sobol_dirVs_array :: Array DIM2 Elem
sobol_dirVs_array = fromList (Z :. sobol_dim :. sobol_bit_count) $ concat sobol_dirVs

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

fromBool :: (Elt a, IsNum a) => Exp Bool -> Exp a
fromBool b = b ? (1, 0)

unindex3 :: (Elt i, IsIntegral i) => Exp DIM3 -> Exp (i, i, i)
unindex3 ix
  = let Z :. i :. j :. k = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
    in  lift (fromIntegral i, fromIntegral j, fromIntegral k)

fst3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
fst3 e = let (x, _:: Exp b, _:: Exp c) = unlift e in x

thd3 :: forall a b c. (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp c
thd3 e = let (_ :: Exp a, _:: Exp b, x) = unlift e in x

-- Manually flattened version of the inductive algorithm for
-- generating sobol sequences
sobolN :: Index -> Acc (Array DIM2 SpecReal)
sobolN n =
  let
    Z :. i :. j = arrayShape sobol_dirVs_array
    cubeSize = constant $ Z :. n :. i :. j

    sobolIndices = generate cubeSize (fst3 . unindex3)
    directionNumberIndices = generate cubeSize (thd3 . unindex3)

    ps = map fromBool $ zipWith (testBit . grayCode) sobolIndices directionNumberIndices

    directionNumbersRep = replicate (constant $ Z :. n :. All :. All) (use sobol_dirVs_array)

    xs :: Acc (Array DIM2 Elem)
    xs = fold1 xor $ zipWith (*) directionNumbersRep ps
  in map ((/sobol_divisor) . fromIntegral) xs

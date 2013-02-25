{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Sobol where

import Prelude hiding (map, zipWith, iterate, (^), replicate)
import qualified Prelude
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Bits
import Data.Typeable (Typeable)

import Data.Array.Repa

type Elem = Word32
type SpecReal = Double
type Index = Int

sobol_bit_count = 30

sobol_divisor :: SpecReal
sobol_divisor = fromIntegral $ (bit 30 :: Int)

sobol_dirVs_array :: Array U DIM1 (Elem)
sobol_dirVs_array = computeUnboxedS $
  fromFunction (Z :. sobol_bit_count)
               (\(Z :. j) -> bit (29-(fromIntegral j)))
grayCode :: Index -> Elem
grayCode n = ((fromIntegral n) `xor` ((fromIntegral n) `shiftR` 1))

unindex1 :: DIM1 -> Int
unindex1 (Z :. i) = i

fromBool :: Num a => Bool -> a
fromBool b = if b then 1 else 0

bitVec :: Elem -> Array U DIM1 Elem
bitVec e = computeUnboxedS $ fromFunction (Z :. sobol_bit_count) (f . unindex1)
 where
   f :: Int -> Elem
   f i = fromBool $ (e .&. bit i) /= 0

sobolInd :: Array U DIM1 Elem -> Index -> SpecReal
sobolInd dirVs ix = normalise $ foldAllS xor 0 xs
  where
    xs :: Array D DIM1 Elem
    xs = zipWith (*) dirVs (bitVec $ grayCode ix)
    
    normalise :: Elem -> SpecReal
    normalise = ((/sobol_divisor ) . fromIntegral)

sobolSequence_ :: Index -> Array U DIM1 SpecReal
sobolSequence_ num_iters = computeUnboxedS $ fromFunction (Z :. num_iters) $ \(Z :. i) -> sobolInd sobol_dirVs_array i


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
import Unfold

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

sobolInd :: Array U DIM1 Elem -> Index -> Elem
sobolInd dirVs ix = foldAllS xor 0 xs
  where
    xs :: Array D DIM1 Elem
    xs = zipWith (*) dirVs (bitVec $ grayCode ix)
    
lsb0_help ell c | (c .&. 1 == 0) = ell
                | otherwise = lsb0_help (ell+1) (c `shiftR` 1)

-- PROBLEM: min{ k | (rep n)[k] == 0}
-- lsb0 :: Index -> Index
lsb0 n = lsb0_help 0 n

sobolRec :: Array U DIM1 Elem -> Index -> Elem -> Elem
sobolRec dirVs i e = e `xor` (dirVs ! (Z :. lsb0 (i-1)))


normalise :: Elem -> SpecReal
normalise = ((/sobol_divisor ) . fromIntegral)

sobolSequence_ :: Monad m => Array U DIM1 Elem -> Index -> m (Array U DIM1 SpecReal)
sobolSequence_ dirVs n = unfoldChunkedP n (sobolRec dirVs) (sobolInd dirVs) normalise

sobolSequence :: Monad m => Index -> m (Array U DIM1 SpecReal)
sobolSequence = sobolSequence_ sobol_dirVs_array



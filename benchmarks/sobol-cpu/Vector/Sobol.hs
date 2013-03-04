module Sobol where

import System.Environment

import Data.Word
import Data.Bits
import Data.Vector.Unboxed
import qualified Data.Vector as VB
import Prelude hiding (map, zipWith, foldl1, replicate)
import qualified Prelude as P

type Elem = Word32
type SpecReal = Double
type Index = Int

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor = fromIntegral (2^30)
sobol_dirVs :: VB.Vector (Vector Elem)
sobol_dirVs = VB.fromList [fromList [2^k | k <- [29,28..0]]]

grayCode :: Index -> Elem
grayCode ix = fromIntegral (ix `xor` (ix `shiftR` 1))

fromBool :: Num a => Bits a => Bool -> a
fromBool True = 1
fromBool False = 0

bitVec :: Elem -> Vector Elem
bitVec e = generate sobol_bit_count f
  where
    f :: Int -> Elem
    f i = fromBool $ (e .&. bit i) /= 0

sobolInd :: Vector Elem -> Index -> Elem
sobolInd dirVs ix = foldl1 xor $ zipWith (*) dirVs (bitVec $ grayCode ix)

normalise :: Elem -> SpecReal
normalise = ((/sobol_divisor ) . fromIntegral)

-- As ffs in C. Finds the least set bit. (ffs(0) = 0, ffs(1) = 1)
ffs 0 = 0
ffs x = ffs' 1 x
  where
    ffs' count x | x .&. 1 == 1 = count
                 | otherwise = ffs' (count+1) (x `shiftR` 1)

sobolRec :: Vector Elem -> Index -> Elem -> Elem
sobolRec dirVs i e = e `xor` (dirVs ! (ffs i - 1))

-- sobolSequence :: Index -> VB.Vector SpecReal
-- sobolSequence num_iters = VB.generate num_iters (sobolInd (sobol_dirVs VB.! 0))

sobolSequence_ :: Vector Elem -> Index -> Vector SpecReal
sobolSequence_ dirVs n = unfoldrN n step (1, first)
  where
    first = 0 --sobolInd dirVs 1
    step (i,x) = let x' = sobolRec dirVs i x
                 in Just (normalise x', (i+1, x'))
    

sobolSequence :: Index -> Vector SpecReal
sobolSequence = sobolSequence_ (sobol_dirVs VB.! 0)

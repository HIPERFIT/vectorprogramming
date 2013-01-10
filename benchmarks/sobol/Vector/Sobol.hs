module Sobol where

import System.Environment

import qualified Data.Word as DW
import Data.Bits

import Data.Vector.Unboxed
import qualified Data.Vector as VB
import Prelude hiding (map, zipWith, foldl1)
import qualified Prelude as P

type Elem = DW.Word32
type SpecReal = Double
type Index = Int

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor = fromIntegral (2^30)
sobol_dirVs = VB.fromList [fromList [2^k | k <- [29,28..0]]]

grayCode :: Index -> Elem
grayCode ix = fromIntegral (ix `xor` (ix `shiftR` 1))

bitVec :: Elem -> Vector Elem
bitVec = unfoldrN sobol_bit_count (\e -> Just (e .&. 1, e `shiftR` 1))

sobolInd :: Vector Elem -> Index -> SpecReal
sobolInd dirVs ix = normalise $ foldl1 xor $ zipWith (*) dirVs (bitVec $ grayCode ix)
  where
    normalise :: Elem -> SpecReal
    normalise = ((/sobol_divisor ) . fromIntegral)

sobolSequence :: Index -> VB.Vector SpecReal
sobolSequence num_iters = VB.generate num_iters (sobolInd (sobol_dirVs VB.! 0))

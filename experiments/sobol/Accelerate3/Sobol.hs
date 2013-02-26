{-# LANGUAGE TypeOperators #-}

module Sobol where

import System.Environment

import qualified Data.Word as DW
import Data.Bits hiding (shiftR, testBit,bit)
import qualified Data.List as DL
import qualified Data.Vector as VB

import Prelude hiding (map, filter, fromIntegral, zipWith, replicate, length, zip, fst, snd)
import qualified Prelude

import Data.Array.Accelerate hiding (Elem)

type Elem = DW.Word32
type SpecReal = Double
type Index = Int

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor :: Exp SpecReal
sobol_divisor = Prelude.fromIntegral (2^30)
sobol_dirVs = [ [2^k | k <- [29,28..0]]]

sobol_dirVs_array :: Acc (Array DIM1 Elem)
sobol_dirVs_array = generate (index1 sobol_bit_count) (\j -> bit (29-(fromIntegral $ unindex1 j)))

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

fromBool :: (Elt a, IsNum a) => Exp Bool -> Exp a
fromBool b = b ? (1, 0)

bitVec :: Exp Elem -> Acc (Array DIM1 Elem)
bitVec e = generate (index1 sobol_bit_count) (f . unindex1)
 where
   f :: Int -> Elem
   f i = fromBool $ (e .&. bit i) /=* 0
  
  -- map (fromBool . (/=* 0)) $ zipWith (.&.) pow2s $ generate (index1 sobol_bit_count) (const e)
  -- where
  --   pow2s :: Acc (Array DIM1 Elem)
  --   pow2s = generate (index1 sobol_bit_count) (bit . fromIntegral . unindex1)

sobolInd :: Acc (Array DIM1 Elem) -> Exp Index -> Acc (Scalar SpecReal)
sobolInd dirVs ix = map normalise $ fold xor 0 xs
  where
    xs :: Acc (Array DIM1 Elem)
    xs = zipWith (*) dirVs (bitVec $ grayCode ix)
    
    normalise :: Exp Elem -> Exp SpecReal
    normalise = ((/sobol_divisor ) . fromIntegral)

mapsobolInd :: Exp Index -> Acc (Scalar SpecReal)
mapsobolInd n = sobolInd sobol_dirVs_array n

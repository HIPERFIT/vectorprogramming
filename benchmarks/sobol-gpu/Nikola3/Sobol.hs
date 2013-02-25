{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Sobol where

import Prelude hiding (map, zipWith, iterate, fromIntegral, (^), replicate)
import qualified Prelude
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Typeable (Typeable)

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Combinators

type Elem = Word32
type SpecReal = Double
type Index = Int32

sobol_bit_count = 30

sobol_divisor :: Exp SpecReal
sobol_divisor = fromInt $ bit 30

sobol_dirVs_array :: Array D DIM1 (Exp Elem)
sobol_dirVs_array = fromFunction (Z :. sobol_bit_count)
                                 (\(Z :. j) -> bit (29-(fromInt j)))

grayCode :: Exp Index -> Exp Elem
grayCode n = ((fromIntegral n) `xor` ((fromIntegral n) `shiftR` 1))

-- This one appears to be problematic at times!
fromIntegral :: (IsIntegral CUDA a, IsNum CUDA b) => Exp a -> Exp b
fromIntegral = fromInt . toInt

bit x = 1 `shiftL` x

fold :: Shape ix =>
        Source r a =>
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

fromBool :: Unlift CUDA (Exp a) => Num (Exp a) => Exp Bool -> Exp a
fromBool b = if b then 1 else 0

bitVec :: Exp Elem -> Array D DIM1 (Exp Elem)
bitVec e = map (fromBool . (/=* 0)) $ zipWith (&*) pow2s $ replicate sobol_bit_count e
  where
    pow2s :: Array D DIM1 (Exp Elem)
    pow2s = fromFunction (Z :. sobol_bit_count) (\(Z :. i) -> bit $ fromInt i)

toExp :: IsElem (Exp a) => Array D DIM0 (Exp a) -> Exp a
toExp arr = lookup Z
  where
    (Z, lookup) = toFunction arr

sobolInd :: Array D DIM1 (Exp Elem) -> Exp Index -> Exp SpecReal
sobolInd dirVs ix = toExp $ map normalise $ fold xor 0 xs
  where
    xs :: Array D DIM1 (Exp Elem)
    xs = zipWith (*) dirVs (bitVec $ grayCode ix)
    
    normalise :: Exp Elem -> Exp SpecReal
    normalise = ((/sobol_divisor ) . fromIntegral)

sobolSequence_ :: Exp Index -> Array D DIM1 (Exp SpecReal)
sobolSequence_ num_iters = fromFunction (Z :. num_iters) $ \(Z :. i) -> sobolInd sobol_dirVs_array i

-- fold1 :: Shape ix =>
--         Source r a =>
--         IsArray r a =>
--         IsElem a => 
--         Unlift CUDA a =>
--         IsElem (Exp (Lifted CUDA a)) =>
--         Typeable (Lifted CUDA a) =>
--      (a -> a -> a) -> Array r (ix :. Exp Ix) a -> Array D ix a
-- fold1 f arr = 
--   let (sh :. n, lookup) = toFunction arr
--       buildArray ix = snd $ iterate (n-1) iter (1, lookup (sh :. 0))
--         where
--           iter (i,x) = (i+1, f (lookup (ix :. i+1)) x)
--   in fromFunction sh buildArray

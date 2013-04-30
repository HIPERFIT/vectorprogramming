{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Pi where

import Prelude hiding (map, zipWith, iterate, fromIntegral, (^), replicate)
import qualified Prelude
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Typeable (Typeable)

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Combinators

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R

sobol_bit_count = 30

sobol_divisor :: Exp Double
sobol_divisor = fromInt $ bit 30

-- No reasonable way is provided in Nikola to produce literal arrays, so we
-- pass this to sobolND as a device resident vector, transferred before we
-- start measuring
--sobol_dirVs :: (Array G DIM2 (Exp Word32))
sobol_dirVs :: R.Array R.CUF R.DIM2 Word32
sobol_dirVs = R.fromList (R.Z R.:. 32 R.:. 2) 
  [2147483648,1073741824,2684354560,1342177280,
  2281701376,603979776,301989888,754974720,
  1988100096,2654994432,136314880,1678770176,
  2988965888,2098462720,4272029696,3125346304,
  438599680,1226522624,3300237312,3816001536,
  4135585792,3728737280,2820672000,873465088,
  975702144,1494483520,3970040096,2538144464,
  1822721896,3613084132,3432358018,2271450689,

  2147483648,1073741824,3758096384,2952790016,
  2550136832,2483027968,2315255808,1526726656,
  864026624,3653238784,1914699776,1058013184,
  3250061312,2800484352,1401290752,703922176,
  171606016,455786496,3549618176,1778348032,
  3929540608,2871788544,1269173760,4259646208,
  1610779008,4026976576,2016733344,605713840,
  305826616,3475687836,3113412898,2197780721]

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

bitVec :: Exp Word32 -> Array D DIM1 (Exp Word32)
bitVec e = map (fromBool . (/=* 0)) $ zipWith (&*) pow2s $ replicate sobol_bit_count e
  where
    pow2s :: Array D DIM1 (Exp Word32)
    pow2s = fromFunction (Z :. sobol_bit_count) (\(Z :. i) -> bit $ fromInt i)

the :: IsElem (Exp a) => Array D DIM0 (Exp a) -> Exp a
the arr = lookup Z
  where
    (Z, lookup) = toFunction arr

sobolInd :: Array D DIM1 (Exp Word32) -> Exp Int32 -> Exp Double
sobolInd dirVs ix = normalise $ the $ fold xor 0 xs
  where
    xs :: Array D DIM1 (Exp Word32)
    xs = zipWith (*) dirVs (bitVec (fromIntegral ix))

    normalise :: Exp Word32 -> Exp Double
    normalise = ((/sobol_divisor ) . fromIntegral)

{-
sobolSequence_ :: Exp Int32 -> Array D DIM1 (Exp Double)
sobolSequence_ num_iters = fromFunction (Z :. num_iters) $ \(Z :. i) -> sobolInd sobol_dirVs_array i
-}
sobolND :: Array G DIM2 (Exp Word32) -> Exp Int32 -> Array D DIM2 (Exp Double)
sobolND dirVs n =
  fromFunction (Z:.width:.dims) $ \ (Z:.i:.dim) ->
    -- Extract the direction vector of dim
    let dirVsDim = fromFunction (Z:.width) $ \ (Z:.j) -> dirVs ! (Z:.j:.dim)
    in sobolInd dirVsDim i
  where
    (Z :. width :. dims) = extent dirVs
  
-- pi2d :: Acc (Array DIM2 Double) -> Exp Double
pi2d :: Array D DIM2 (Exp Double) -> Exp Double
pi2d nums = 4 * (error "look in comments for clues")
               {-fromInt n-} {-((fromInt $ (n - (the $ fold (+) 0 dists))) / (fromInt n))-}
               -- both of the curly brace comments above fail with the same "cannot compile"-error.
               -- It's quite unclear why though.
 where
   Z:. n :. _  = extent $ nums
   dists :: Array D DIM1 (Exp Int32)
   dists = fromFunction (Z :. n) 
              (\ (Z:.ix) ->
                      fromBool $ (1 <=*) $ sqrt $ (nums ! (Z :. ix :. (0 :: Exp Int32))) ** 2
                                      + (nums ! (Z :. ix :. (1 :: Exp Int32))) ** 2)

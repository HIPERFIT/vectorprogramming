{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol where

import System.Environment
import Prelude hiding (map, filter, zipWith, zip, iterate, fromIntegral, (^), length, replicate)
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

replicateAddDIM :: Source r a => Shape sh => Exp Ix -> Array r sh a -> Array D (sh :. Exp Ix) a
replicateAddDIM n arr =
  let (sh, lookup) = toFunction arr
  in fromFunction (sh :. n) $ \(sh' :. i) -> lookup sh'

replicateAddFirstDIM :: Source r a => Exp Ix -> Array r DIM2 a -> Array D DIM3 a
replicateAddFirstDIM n arr =
  let (Z :. l :. m, lookup) = toFunction arr
  in fromFunction (Z :. n :. l :. m) $ \(Z :. _ :. i :. j) -> lookup (Z :. i :. j)

type DIM3 = DIM2 :. Exp Ix

mapsobolInd_ :: Exp Int32 -> Array D DIM2 (Exp Elem)
mapsobolInd_ lengthn =
  let
    ns = fromFunction (Z :. lengthn) (\(Z :. i) -> i)
    
    indices :: Array D DIM2 (Exp Ix)
    indices = fromFunction (extent sobol_dirVs_array)
                           (\(Z :. i :. j) -> j)
    
    (Z :. dirVs_n :. dirVs_m) = extent sobol_dirVs_array
    
    indicesRep :: Array D DIM3 (Exp Index)
    indicesRep = replicateAddFirstDIM (lengthn) indices

    nss :: Array D DIM3 (Exp Index)
    nss = replicateAddDIM dirVs_m $ replicateAddDIM dirVs_n ns

    ps :: Array D DIM3 (Exp Bool)
    ps = zipWith (testBit . fromIntegral . grayCode) nss indicesRep

    dirVsRep :: Array D DIM3 (Exp Elem)
    dirVsRep = replicateAddFirstDIM (lengthn) sobol_dirVs_array

    doit :: (Exp Elem, Exp Bool) -> (Exp Elem, Exp Bool) -> (Exp Elem, Exp Bool)
    doit a@(xa,ia) b@(xb,ib) =
         if ia then if ib then (lift $ xa `xor` xb, lift ia)
                    else a
         else b
              
    z :: Array D DIM3 (Exp Elem, Exp Bool)
    z = zip dirVsRep ps
    
    f :: Array D DIM2 (Exp Elem, Exp Bool)
    f = fold doit (lift (0 :: Elem), lift True) z
  in map fst f

mapsobolInd :: Exp Int32 -> Array D DIM2 (Exp SpecReal)
mapsobolInd lengthn = map ((/sobol_divisor) . fromIntegral) $ mapsobolInd_ lengthn

mapsobolIndr :: Exp Int32 -> Array D DIM1 (Exp SpecReal)
mapsobolIndr lengthn = reshape (Z :. lengthn) $ mapsobolInd lengthn
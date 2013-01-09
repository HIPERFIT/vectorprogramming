{-# LANGUAGE TypeOperators #-}

-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol where

import System.Environment

import qualified Data.Word as DW
import Data.Bits hiding (shiftR, testBit)
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

sobol_dirVs_array :: Array DIM2 Elem
sobol_dirVs_array = fromList (Z :. Prelude.length sobol_dirVs :. Prelude.length (head sobol_dirVs)) 
                    $ Prelude.concat sobol_dirVs

length :: Shape sh => Elt a => Array (sh :. Int) a -> Int
length arr = let (_ :. n) = arrayShape arr in n

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

-- Manually flattened version
mapsobolInd_ :: Index -> Acc (Array DIM2 Elem)
mapsobolInd_ n =
  let
    
    ns = generate (index1 $ constant n) unindex1
    
    indices :: Acc (Array DIM2 Index)
    indices = generate (constant $ arrayShape sobol_dirVs_array) 
                       (snd . unindex2)
    
    indicesRep = replicate (constant $ Z :. n :. All :. All) indices
    
    Z :. i :. j = arrayShape sobol_dirVs_array
    
    nss = replicate (constant $ Z :. All :. i :. j) ns

    ps = zipWith (testBit . grayCode) nss indicesRep
    
    dirVsRep = replicate (constant $ Z :. n :. All :. All) (use sobol_dirVs_array)
    
    doit :: Exp (Elem, Bool) -> Exp (Elem, Bool) -> Exp (Elem, Bool)
    doit a b =
      let xa,xb :: Exp Elem
          ia,ib :: Exp Bool
          (xa,ia) = (fst a, snd a)
          (xb,ib) = (fst b, snd b)
      in ia ? 
           (ib ?
              (lift (xa `xor` xb, ia),
               a),
            b)
  in map fst $ fold doit (constant (0, True)) $ zip dirVsRep ps

mapsobolInd :: Index -> Acc (Array DIM2 SpecReal)
mapsobolInd n = map ((/sobol_divisor) . fromIntegral) $ mapsobolInd_ n

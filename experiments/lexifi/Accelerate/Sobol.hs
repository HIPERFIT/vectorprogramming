{-# LANGUAGE TypeOperators #-}

-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol (sobol) where

import Prelude hiding (map, filter, fromIntegral, zipWith, replicate, length, zip, fst, snd)
import qualified Prelude
import Data.Bits hiding (shiftR, testBit)

import Data.Array.Accelerate hiding (Elem)
import qualified Data.Vector as V
import PricingTypesS

length :: Shape sh => Elt a => Array (sh :. Int) a -> Int
length arr = let (_ :. n) = arrayShape arr in n

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

-- Manually flattened version
mapsobolInd_ :: Pricing_Data -> Vector Index -> Acc (Array DIM2 Elem)
mapsobolInd_ conf ns =
  let
    sobol_dirVs_array :: Array DIM2 Elem
    sobol_dirVs_array = fromList (Z :. V.length (sobol_dirVs conf) :. V.length (V.head (sobol_dirVs conf))) 
                    $ V.toList $ V.concat $ V.toList (sobol_dirVs conf)

    
    indices :: Acc (Array DIM2 Index)
    indices = generate (constant $ arrayShape sobol_dirVs_array)
                       (snd . unindex2)
    indicesRep :: Acc (Array DIM3 Index)
    indicesRep = replicate (constant $ Z :. length ns :. All :. All) indices
    
    Z :. i :. j = arrayShape sobol_dirVs_array
    
    nss = replicate (constant $ Z :. All :. i :. j) (use ns)
    
    ps :: Acc (Array DIM3 Bool)
    ps = zipWith (testBit . grayCode) nss indicesRep
    
    dirVsRep = replicate (constant $ Z :. length ns :. All :. All) (use sobol_dirVs_array)
    
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

sobol :: Pricing_Data -> Vector Index -> Acc (Array DIM2 SpecReal)
sobol conf ns = map ((/ divisor) . fromIntegral) $ mapsobolInd_ conf ns
  where divisor :: Exp SpecReal
        divisor = constant $ sobol_divisor conf

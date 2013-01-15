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

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

sobolInd_ :: Exp Index -> Acc (Vector Elem)
sobolInd_ n =
  let 
    indices :: Acc (Array DIM2 Index)
    indices = generate (constant $ arrayShape sobol_dirVs_array) 
                       (snd . unindex2)
    
    ps = map (testBit (grayCode n)) indices

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
  in map fst $ fold doit (constant (0, True)) $ zip (use sobol_dirVs_array) ps

sobolInd :: Exp Index -> Acc (Vector SpecReal)
sobolInd n = map norm (sobolInd_ n)
    where
      norm :: Exp Elem -> Exp SpecReal
      norm = ( / sobol_divisor) . fromIntegral


-- -- Previous attempt at generating sobol sequences
-- -- Can not go through because we cannot define the value "sizeAfterFilter"
-- sobolInd_ :: Exp Index -> Acc (Vector Elem)
-- sobolInd_ n = 
--   let indices :: Acc (Vector Index)
--       indices = filter (testBit (grayCode n) . fromIntegral) $
--                 use (fromList (Z :. sobol_bit_count) [0..sobol_bit_count-1])
                
--       indices' :: Acc (Array DIM2 Index)
--       indices' = replicate (constant $ Z :. length sobol_dirVs_array :. All) indices
      
--       arr :: Acc (Vector Elem)
--       arr = gather (flatten indices') (flatten (use sobol_dirVs_array))
      
--       -- Seems hard to acquire this.
--       sizeAfterFilter :: Int
--       sizeAfterFilter = undefined
      
--       arr' :: Acc (Array DIM2 Elem)
--       arr' = reshape (constant $ Z :. length sobol_dirVs_array :. sizeAfterFilter) arr
      
--   in fold xor 0 arr'

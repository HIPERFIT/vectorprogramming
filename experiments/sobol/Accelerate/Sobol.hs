-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol where

import System.Environment

import qualified Data.Word as DW
import qualified Data.Bits as DB
import qualified Data.List as DL
import qualified Data.Vector as VB

import Prelude hiding (map, filter, fromIntegral, zipWith)
import qualified Prelude

import Data.Array.Accelerate hiding (Elem)

type Elem = DW.Word32
type SpecReal = Double
type Index = Int

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor = fromIntegral (2^30)
sobol_dirVs = [ [2^k | k <- [29,28..0]]]

sobol_dirVs_array :: Acc (Array DIM2 Elem)
sobol_dirVs_array = 
  use $ fromList (Z :. length sobol_dirVs :. length (head sobol_dirVs)) 
      $ Prelude.concat sobol_dirVs

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

-- Everything seems to work out except for the filter used below.
-- Can we avoid it? gatherIf?
-- Also, the use of replicate to construct a large index map for
-- gather seems a wasteful.
sobolInd_ :: Exp Index -> Acc (Vector Elem)
sobolInd_ n = 
  let indices :: Acc (Vector Index)
      indices = filter (testBit (grayCode n) . fromIntegral) $
                use (fromList (Z :. sobol_bit_count) [0..sobol_bit_count-1])
      indices' = replicate (Z :. length sobol_dirVs_array :. All) indices
      arr = gather indices sobol_dir_Vs_array
  in fold xor 0 arr

sobolInd :: Exp Index -> Acc (Vector SpecReal)
sobolInd n = map norm (sobolInd_ n)
    where
      norm :: Exp Index -> Exp SpecReal
      norm = ( / constant sobol_divisor) . fromIntegral

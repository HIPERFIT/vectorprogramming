-- ^
-- Sobol sequence generation in Nikola
-- Outset from the Haskell version of the LexiFi code
module Sobol (sobolSequences) where

import System.Environment

import Prelude hiding (map, foldl, filter, zipWith)
import qualified Prelude

import Data.Array.Nikola.Backend.CUDA

-- type Elem = DW.Word32
-- type SpecReal = Double
type Index = Int32

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor = fromIntegral (2^30)
sobol_dirVs = [VB.fromList [2^k | k <- [29,28..0]]]

--- Nikola does not have shifts or xor, for now we just use operators
--- with the right types
xor = (|*)
shiftR = (|*)

grayCode :: Exp Index -> Exp Elem
grayCode n = fromIntegral (n `xor` (n `shiftR` 1))

-- Here we need a way to filter and fold over a sequence, which aren't
-- supplied by Nikola. Further more the fold is nested inside a map.
-- gatherIf from Accelerate seems appropriate, but we will still lack
-- a filter
sobolInd_ :: Index -> [Elem]
sobolInd_ n =
        let indices  = filter (DB.testBit (grayCode n)) [0..sobol_bit_count-1]
            xorVs vs = DL.foldl' DB.xor 0 [vs VB.! i | i <- indices]
        in map xorVs sobol_dirVs

-- Should work in Nikola
sobolInd :: Index -> [SpecReal]
sobolInd n = map norm (sobolInd_ n)
    where norm = ( / sobol_divisor ) . fromIntegral

-- This would require nested loops or perhaps flattening by hand
sobolSequences :: Index -> [[SpecReal]]
sobolSequences num_iters = map sobolInd [1..num_iters]

module Sobol (sobolSequences) where

import System.Environment

import qualified Data.Word as DW
import qualified Data.Bits as DB
import qualified Data.List as DL

import qualified Data.Vector as VB
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU

type Elem = DW.Word32
type SpecReal = Double
type Index = Integer

sobol_bit_count = 30
sobol_dim = 1
sobol_divisor = fromIntegral (2^30)
sobol_dirVs = [VB.fromList [2^k | k <- [29,28..0]]]


lsb0_help ell c | (c DB..&. 1 == 0) = ell
                | otherwise = lsb0_help (ell+1) (c `DB.shiftR` 1)

-- PROBLEM: min{ k | (rep n)[k] == 0}
-- lsb0 :: Index -> Index
lsb0 n = lsb0_help 0 n

grayCode :: Index -> Elem
grayCode n = fromIntegral (n `DB.xor` (n `DB.shiftR` 1))

-- independent formula
-- LENGTH(RET) = LENGTH(sobol_dirVs)
sobolInd_ :: Index -> [Elem]
sobolInd_ n =
        let indices  = filter (DB.testBit (grayCode n)) [0..sobol_bit_count-1]
            xorVs vs = DL.foldl' DB.xor 0 [vs VB.! i | i <- indices]
        in map xorVs sobol_dirVs

sobolInd :: Index -> [SpecReal]
sobolInd n = map norm (sobolInd_ n)
    where norm = ( / sobol_divisor ) . fromIntegral

sobolSequences :: Index -> [[SpecReal]]
sobolSequences num_iters = map sobolInd [1..num_iters]

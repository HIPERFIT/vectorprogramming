{-# LANGUAGE RecordWildCards #-}
module Binom where

import Options
import Prelude                           as P
import Data.Array.Accelerate             as A
import Data.Array.Accelerate.Interpreter as A
import Data.List(foldl1')

type EurOption = (Bool, Float, Float, Int, Float, Float)

toTuple :: EurOpt -> EurOption
toTuple (EurOpt{..}) = ( opttype == Call
                       , s0
                       , strike
                       , expiry
                       , riskless
                       , volatility
                       )

binom :: EurOpt -> Int -> Float
binom opt numSteps = (A.run $ binomAcc numSteps (unit (constant numSteps)) tup) `indexArray` Z
  where
    tup :: Acc (Scalar EurOption)
    tup = unit . constant . toTuple $ opt

binomAcc :: Int -> Acc (Scalar Int) -> Acc (Scalar EurOption) -> Acc (Scalar Float)
binomAcc numSteps numStepsArr opt = unit (first A.! (index1 0))
  where
    numStepsExp = the numStepsArr
    -- Leafs of the binomial tree
    leafs = generate (index1 $ numStepsExp + 1) helper
      where
        helper ix = let Z :. i = unlift ix
                    in s0 * exp(vsdt * A.fromIntegral (2 * i - numStepsExp))

    -- Profits at exercise time
    profit = A.map (\x -> iscall ? (x - strike, strike -x)) leafs
    vFinal  = A.map (A.max 0) profit

    -- Discounting backwards
    stepBack vPrev = A.zipWith back (A.tail vPrev) (A.init vPrev)
      where back x1 x2 = puByr * x1 + pdByr * x2
    first = foldl1' (>->) (P.replicate numSteps stepBack) vFinal

    -- Model and option variables
    iscall :: Exp Bool;  s0 :: Exp Float; strike :: Exp Float;
    expiry :: Exp Int; riskless :: Exp Float; volatility :: Exp Float;
    (iscall, s0, strike, expiry,riskless,volatility) = unlift $ opt A.! (constant Z)

    dt = A.fromIntegral expiry/A.fromIntegral numStepsExp
    vsdt = volatility * sqrt dt
    u = exp(vsdt) 
    d = 1/u
    rr = exp(riskless*dt)
    rrInv = 1.0 / rr
    pu = (rr - d)/(u - d)
    pd = 1.0 - pu
    puByr = pu * rrInv
    pdByr = pd * rrInv

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

binom :: EurOpt -> Float
binom opt = (A.run $ binomAcc tup) `indexArray` Z
  where
    tup :: Acc (Scalar EurOption)
    tup = unit . constant . toTuple $ opt

binomAcc :: Acc (Scalar EurOption) -> Acc (Scalar Float)
binomAcc opt = unit (first A.! (index1 0))
  where
    -- Leafs of the binomial tree
    leafs = generate (lift $ Z :. (numSteps+1)) helper
      where
        helper ix = let Z :. i = unlift ix
                    in s0 * exp(vsdt * A.fromIntegral (2 * i - constant numSteps))

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

    dt = A.fromIntegral expiry/A.fromIntegral (constant numSteps)
    vsdt = volatility * sqrt dt
    u = exp(vsdt) 
    d = 1/u
    rr = exp(riskless*dt)
    rrInv = 1.0 / rr
    pu = (rr - d)/(u - d)
    pd = 1.0 - pu
    puByr = pu * rrInv
    pdByr = pd * rrInv

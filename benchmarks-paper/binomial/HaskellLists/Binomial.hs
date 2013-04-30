{-# LANGUAGE RecordWildCards #-}
module Binomial where

import Data.List (foldl')
import Options

binom :: Int -> EurOpt -> Float
binom numSteps (EurOpt{..}) = head $ foldl' stepBack vFinal [numSteps, numSteps-1 .. 1]
  where
--    leafs = map (\i -> s0 * u^i * d^(numSteps-i)) [0..numSteps]
    leafs = [s0 * exp(vsdt * fromIntegral (2 * i - numSteps)) | i <- [0..numSteps]]
    profit = case opttype of Put  -> map (strike -) leafs
                             Call -> map (flip (-) strike) leafs
    vFinal  = map (max 0) profit
    
    stepBack vPrev _ = zipWith back (tail vPrev) (init vPrev)
      where back x1 x2 = puByr * x1 + pdByr * x2

    u,d,dt :: Float
    dt = fromIntegral expiry/fromIntegral numSteps
    vsdt = volatility * sqrt dt
    u = exp(vsdt) 
    d = 1/u
    rr = exp(riskless*dt)
    rrInv = 1.0 / rr
    pu = (rr - d)/(u - d)
    pd = 1.0 - pu
    puByr = pu * rrInv
    pdByr = pd * rrInv

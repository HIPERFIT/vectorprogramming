{-# LANGUAGE RecordWildCards #-}
module Binomial where

import Data.List (foldl')
import Options

binom :: EurOpt -> Float
binom (EurOpt{..}) = head first
  where
    -- Leafs of binomial tree
    leafs = [s0 * exp(vsdt * fromIntegral (2 * i - numSteps))
            | i <- [0..numSteps :: Int]]

    -- Profits at the final stage
    profit = case opttype of Put  -> map (strike -) leafs
                             Call -> map (flip (-) strike) leafs
    vFinal  = map (max 0) profit
    
    -- Discounting backwards
    stepBack vPrev _ = zipWith back (tail vPrev) (init vPrev)
      where back x1 x2 = puByr * x1 + pdByr * x2
    first = foldl' stepBack vFinal [1..numSteps :: Int]

    -- Model and option variables
    u,d,dt,vsdt,rr,rrInv,pu,pd,puByr,pdByr :: Float
    dt = fromIntegral expiry/fromIntegral (numSteps :: Int)
    vsdt = volatility * sqrt dt
    u = exp(vsdt) 
    d = 1/u
    rr = exp(riskless*dt)
    rrInv = 1.0 / rr
    pu = (rr - d)/(u - d)
    pd = 1.0 - pu
    puByr = pu * rrInv
    pdByr = pd * rrInv

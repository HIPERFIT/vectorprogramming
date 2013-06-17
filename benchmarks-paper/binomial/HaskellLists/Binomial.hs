module Binomial where

import Options

binom :: Int -> Option -> Float
binom n (s0,strike,expiry,riskless,volatility) = 
   head $ foldl stepBack vFinal [1..n]
 where
  -- Model and option variables
  dt = fromIntegral expiry/fromIntegral n
  vsdt = volatility * sqrt dt

  -- Leafs of binomial tree
  leafs = [s0 * exp(vsdt * fromIntegral (2*i-n))
          | i <- [0..n]]

  -- Profits at the final stage
  vFinal :: [Float]
  vFinal = map (\x -> max 0 $ x - strike) leafs

  -- More model and option variables
  rr = exp(riskless*dt)
  rrInv = 1.0 / rr
  u = exp(vsdt)         ; d = 1/u
  pu = (rr - d)/(u - d) ; pd = 1.0 - pu
  puByr = pu * rrInv    ; pdByr = pd * rrInv

  -- Discounting one step backwards
  stepBack :: [Float] -> a -> [Float]
  stepBack vPrev _ = zipWith back (tail vPrev)
                                  (init vPrev)
    where back x1 x2 = puByr * x1 + pdByr * x2

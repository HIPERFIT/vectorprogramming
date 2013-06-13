module Implementation (
    vFinal,
    stepBack
  ) where

import Prelude hiding (init, map, max, tail, zipWith)
import Data.Array.Nikola.Backend.CUDA
import Data.Int

vFinal :: Exp Int32
       -> Exp Float
       -> Exp Float
       -> Exp Int32
       -> Exp Float
       -> Exp Float
       -> Vector D (Exp Float)
vFinal numSteps s0 strike expiry _ volatility = map (max 0) profit
  where
    -- Leafs of the binomial tree
    leafs = fromFunction (Z :. (numSteps+1)) helper
      where
        helper (Z :. i) = s0 * exp(vsdt * fromInt (2 * i - numSteps))

    -- Profits at exercise time
    profit = map (\x -> x - strike) leafs

    dt = fromInt expiry/ fromInt numSteps
    vsdt = volatility * sqrt dt

stepBack :: Exp Int32
         -> Exp Float
         -> Exp Float
         -> Exp Int32
         -> Exp Float
         -> Exp Float
         -> Vector G (Exp Float)
         -> Exp Int32
         -> Vector D (Exp Float)
stepBack numSteps s0 strike expiry riskless volatility vPrev i =
         zipWith back (tail vPrev) (init vPrev)
  where
    back x1 x2 = puByr * x1 + pdByr * x2

    -- Model and option variables
    dt = fromInt expiry/fromInt numSteps
    vsdt = volatility * sqrt dt
    u = exp(vsdt) 
    d = 1/u
    rr = exp(riskless*dt)
    rrInv = 1.0 / rr
    pu = (rr - d)/(u - d)
    pd = 1.0 - pu
    puByr = pu * rrInv
    pdByr = pd * rrInv

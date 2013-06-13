module Binom where

import Options
import Prelude                           as P
import Data.Array.Accelerate             as A
import Data.Array.Accelerate.CUDA        as A
import Data.List(foldl1')

run1binom :: Option -> Int -> Float
run1binom opt numSteps = (run1 (binomAcc numSteps) tuparr) `indexArray` Z
 where
   tuparr :: Scalar (Option, Int)
   tuparr = fromList Z [(opt, numSteps)]

runbinom :: Option -> Int -> Float
runbinom opt numSteps = (run $ binomAcc numSteps (use tuparr)) `indexArray` Z
 where
   tuparr :: Scalar (Option, Int)
   tuparr = fromList Z [(opt, numSteps)]

-- Cdall option pricer
binomAcc :: Int -> Acc (Scalar (Option, Int)) -> Acc (Scalar Float)
binomAcc numSteps opt_numSteps = unit (first A.! (index1 0))
  where
    opt :: Exp Option
    numStepsExp :: Exp Int
    (opt, numStepsExp) = unlift (the opt_numSteps)
    -- Leafs of the binomial tree
    leafs = generate (index1 $ numStepsExp + 1) helper
      where
        helper ix = let Z :. i = unlift ix
                    in s0 * exp(vsdt * A.fromIntegral (2 * i - numStepsExp))

    -- Profits at exercise time
    profit = A.map (\x -> x - strike) leafs
    vFinal  = A.map (A.max 0) profit

    -- Discounting backwards
    stepBack vPrev = A.zipWith back (A.tail vPrev) (A.init vPrev)
      where back x1 x2 = puByr * x1 + pdByr * x2
    first = foldl1' (>->) (P.replicate numSteps stepBack) vFinal

    -- Model and option variables
    s0 :: Exp Float; strike :: Exp Float; expiry :: Exp Int;
    riskless :: Exp Float; volatility :: Exp Float;
    (s0, strike, expiry,riskless,volatility) = unlift $ opt

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

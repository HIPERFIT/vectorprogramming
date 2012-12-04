module American.Nikola.Implementation (
    finalPut,
    prevPut,
    binom
  ) where

import Prelude hiding (drop, init, map, max, min, tail, take, zipWith, (^), reverse)
import Data.List (foldl')

import Data.Int

import Data.Array.Nikola.Backend.CUDA

--
-- This code is adapted from Ken Friis Larsen's implementation of pricing for
-- American options available at <http://github.com/kfl/american-options>.
--

type F = Double

v1 ^*^ v2 = zipWith (*) v1 v2
v1 ^+^ v2 = zipWith (+) v1 v2
c -^ v = map (c -) v
c *^ v = map (c *) v

pmax v c = map (max c) v
ppmax = zipWith max

-- standard econ parameters
strike = 100
bankDays = 252
s0 = 100
r = 0.03; alpha = 0.07; sigma = 0.20

finalPut :: Vector D (Exp F)
         -> Vector D (Exp F)
         -> Vector D (Exp F)
finalPut uPow dPow = pmax (strike -^ st) 0
  where
    st :: Vector D (Exp F)
    st = s0 *^ (uPow ^*^ dPow)

prevPut :: Vector D (Exp F)
        -> Vector D (Exp F)
        -> Exp Int32
        -> Vector D (Exp F)
        -> Exp Int32
        -> Vector D (Exp F)
prevPut uPow dPow expiry put i =
    ppmax(strike -^ st) ((qUR *^ tail put) ^+^ (qDR *^ init put))
  where
    st = s0 *^ ((take i uPow) ^*^ (drop (n+1-i) dPow))

    n = expiry*bankDays
    dt = fromInt expiry/fromInt n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

binom :: Int -> Vector D (Exp F)
binom expiry = first
  where
    uPow = generate n' (\i -> u ** fromIntegral i)

    dPow = reverse $ generate n' (\i -> d ** fromIntegral i)

    first = foldl' (\x i -> prevPut uPow dPow expiry' x (fromIntegral i)) (finalPut uPow dPow)
              [n, n-1 .. 1]

    -- standard econ parameters
    bankDays = 252
    alpha = 0.07; sigma = 0.20

    expiry' :: Exp Ix
    expiry' = fromIntegral expiry
    n' :: Exp Ix
    n' = fromIntegral $ n +1

    n = expiry * bankDays
    dt = (fromIntegral expiry :: Exp F)/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)

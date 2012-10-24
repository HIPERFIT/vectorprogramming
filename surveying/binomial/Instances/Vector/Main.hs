module Main
--        (binom, main)
where
import qualified Data.Vector.Unboxed as V

import Data.List(foldl')
import System.Environment(getArgs)

import BinomialBenchmark.Main(runTest)


-- Pointwise manipulation of vectors and scalars
v1 ^*^ v2 = V.zipWith (*) v1 v2
v1 ^+^ v2 = V.zipWith (+) v1 v2
c -^ v = V.map (c -) v
c *^ v = V.map (c *) v

pmax v c = V.map (max c) v
ppmax = V.zipWith max


binom :: Int -> Double
binom expiry = V.head first
  where
    uPow = V.generate (n+1) (u^)
    dPow = V.reverse $ V.generate (n+1) (d^)

    st = s0 *^ (uPow ^*^ dPow)
    finalPut = pmax (strike -^ st) 0

-- for (i in n:1) {
--   St<-S0*u.pow[1:i]*d.pow[i:1]
--   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }
    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut put i = ppmax (strike -^ st) ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      where st = s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow))

    -- standard econ parameters
    strike = 100 :: Double
    bankDays = 256 :: Int
    s0 = 100 :: Double
    r = 0.03; alpha = 0.07; sigma = 0.20
    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

{-
main = do
  print $ binom 60
-}

main = runTest binom

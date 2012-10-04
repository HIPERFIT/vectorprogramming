{-# OPTIONS_GHC -fvectorise #-}
{-# LANGUAGE ParallelArrays #-}
module BinomialImplDPH(binomWrap) where
{-


module Main
--        (binom, main)
where

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
    strike = 100
    bankDays = 252
    s0 = 100
    r = 0.03; alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

main = runTest binom

 -}
-- DPH incantations
import GHC.Exts
import qualified Data.Vector as V

import qualified Prelude as P hiding (length)

-- Only in vectorised code
import Data.Array.Parallel
import qualified Data.Array.Parallel.PArray as PA
import qualified Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.Prelude.Int as I

{-# INLINE (^*^) #-}
{-# INLINE (^+^) #-}
{-# INLINE (-^) #-}
{-# INLINE (*^) #-}
v1 ^*^ v2 = zipWithP (D.*) v1 v2
v1 ^+^ v2 = zipWithP (D.+) v1 v2
c -^ v = mapP (c D.-) v
c *^ v = mapP (c D.*) v

pmax v c = mapP (D.max c) v
ppmax = zipWithP D.max

headP :: [:D.Double:] -> D.Double
headP arr = indexP arr 0

binom :: I.Int -> D.Double
binom expiry = headP uPow -- first
  where
    -- probably have to lift these definitions out.
    -- uPow = fromPArrayP P.. PA.fromVector P.$ V.generate (n I.+ 1) (u P.^)
    uPow = ...
    dPow = fromPArrayP P.. PA.fromVector P.$ V.reverse P.$ V.generate (n I.+ 1) (d P.^)


    dt = D.fromInt expiry D./ D.fromInt n
    u = D.exp(alpha D.* dt D.+ sigma D.* D.sqrt dt)
    d = D.exp(alpha D.* dt D.- sigma D.* D.sqrt dt)

    -- standard model parameters
    strike = 100 :: D.Double
    bankDays = 252 :: I.Int
    n = expiry I.* bankDays
    s0 = 100 :: D.Double
    r = 0.03; alpha = 0.07; sigma = 0.20
{-
    st = s0 *^ (uPow ^*^ dPow)
    finalPut = pmax (strike -^ st) 0

-- for (i in n:1) {
--   St<-S0*u.pow[1:i]*d.pow[i:1]
--   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }
    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut put i = ppmax (strike -^ st) ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      where st = s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow))


    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

amePEiter :: D.Double -> (a -> a) -> a -> a
amePEiter n f e = if n D.> 0
                  then amePEiter (n D.- 1) f (f e)
                  else e
-}
-- | Pricing of an American Put Option, using the explicit scheme.
-- A direct translation from "Claus Munk" introduction book, aimed at being
-- "embarrasingly correct".
{-
amePutExplicit :: Double -> Double
amePutExplicit years
  amePEiter (maxT D./ deltaT) step endF
    where
      -- compute f_j,n-1. Add the boundaries appropriately
      {-# INLINE step #-}
      step :: [:Double:] -> [:Double:]
      step f_n =     (alpha *^ f_jminus1_n)
                 ^+^ (beta  *^ f_n)
                 ^+^ (gamma *^ f_jplus1_n)

        where f_jminus1_n = appendP (singletonP boundx0)
                                    (sliceP 0 ((lengthP f_n) PI.- 1) f_n)
              f_jplus1_n  = appendP (sliceP 1 ((lengthP f_n) PI.- 1) f_n)
                                    (singletonP boundJ)

    -- Constants
      boundJ  = 0.0
      boundx0 = k D.* exp (0 D.- r) -- slightly simplified relative to Munk, where r is
                              -- time dependent.
      alpha   = 0.5 D.* deltaT D.* (sigmaSq D.- (mu) D./ deltaX)
      beta    = 1 D.- deltaT D.* (r D.+ sigmaSq)
      gamma   = 0.5 D.* deltaT D.* (sigmaSq D.+ mu/deltaX)
      sigmaSq = (sigma D./ deltaX) D.** 2

      sigma   = args !: 0
      mu      = args !: 1
      r       = args !: 2
      k       = args !: 3
      deltaX  = args !: 4
      maxX    = args !: 5
      deltaT  = args !: 6
      maxT    = args !: 7
-}
binomWrap :: Int -> Double
binomWrap years = binom years
  {-
  let argsP = fromPArrayP args in
  if (lengthP argsP) PI.== 8 then
    toPArrayP (amePutExplicit (argsP) (fromPArrayP endF))
  else
    toPArrayP [::]
    -}

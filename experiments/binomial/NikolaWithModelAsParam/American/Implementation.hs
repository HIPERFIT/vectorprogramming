{-# LANGUAGE RecordWildCards #-}
module American.Implementation (
    finalPut, prevPut, finalPut', prevPut'
  ) where

import Data.Int

import Prelude hiding (drop, init, map, tail, take, zipWith, max)
import Data.Array.Nikola.Backend.CUDA

import American.Model

liftInt :: Int -> Exp Int32
liftInt = lift . (fromIntegral :: Int -> Int32)

v1 ^*^ v2 = zipWith (*) v1 v2
v1 ^+^ v2 = zipWith (+) v1 v2
c -^ v = map (c -) v
c *^ v = map (c *) v

pmax v c = map (max c) v
ppmax = zipWith max


finalPut :: Model
         -> Vector G (Exp F)
         -> Vector G (Exp F)
         -> Vector D (Exp F)
finalPut (Model{..}) = finalPut' (lift strike) (lift s0)

finalPut' :: Exp F
          -> Exp F
          -> Vector G (Exp F)
          -> Vector G (Exp F)
          -> Vector D (Exp F)
finalPut' strike s0 uPow dPow = pmax (strike -^ st) 0
  where
    st :: Vector D (Exp F)
    st = s0 *^ (uPow ^*^ dPow)

prevPut :: Model
        -> Vector G (Exp F)
        -> Vector G (Exp F)
        -> Exp Int32
        -> Vector G (Exp F)
        -> Exp Int32
        -> Vector D (Exp F)
prevPut (Model{..}) =
  prevPut' (lift strike) (lift s0)
           (liftInt bankDays)
           (lift alpha) (lift sigma) (lift r)

prevPut' :: Exp F
         -> Exp F
         -> Exp Int32
         -> Exp F
         -> Exp F
         -> Exp F
         -> Vector G (Exp F)
         -> Vector G (Exp F)
         -> Exp Int32
         -> Vector G (Exp F)
         -> Exp Int32
         -> Vector D (Exp F)
prevPut' strike s0 bankDays alpha sigma r uPow dPow expiry put i =
    ppmax(strike -^ st) ((qUR *^ tail put) ^+^ (qDR *^ init put))
  where
    st = s0 *^ ((take i uPow) ^*^ (drop (n+1-i) dPow))
    n = expiry * bankDays
    dt = fromInt expiry/fromInt n
    u = exp(alpha*dt + sigma*sqrt dt)
    d = exp(alpha*dt - sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

{-# LANGUAGE RecordWildCards #-}
module American.Implementation (
    finalPut, prevPut
  ) where

import Data.Int

import Prelude hiding (drop, init, map, tail, take, zipWith)
import Data.Array.Nikola.Backend.CUDA

import American.Model

liftInt :: Int -> Exp Int32
liftInt = lift . fromIntegral

v1 ^*^ v2 = zipWith (*) v1 v2
v1 ^+^ v2 = zipWith (+) v1 v2
c -^ v = map (c -) v
c *^ v = map (c *) v

pmax v c = map (max c) v
ppmax = zipWith max


finalPut :: Model 
         -> Vector M (Exp F)
         -> Vector M (Exp F)
         -> Vector D (Exp F)
finalPut (Model{..}) uPow dPow = pmax (lift strike -^ st) 0
  where
    st :: Vector D (Exp F)
    st = lift s0 *^ (uPow ^*^ dPow)

prevPut :: Model
        -> Vector M (Exp F)
        -> Vector M (Exp F)
        -> Vector M (Exp F)
        -> Exp Int32
        -> Vector D (Exp F)
prevPut (Model{..}) uPow dPow put i =
    ppmax(lift strike -^ st) ((qUR *^ tail put) ^+^ (qDR *^ init put))
  where
    st = lift s0 *^ ((take i uPow) ^*^ (drop (n+1-i) dPow))
    n = liftInt expiry * liftInt bankDays
    dt = fromInt (liftInt expiry)/fromInt n
    u = exp(lift alpha*dt + lift sigma*sqrt dt)
    d = exp(lift alpha*dt - lift sigma*sqrt dt)
    stepR = exp(lift r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

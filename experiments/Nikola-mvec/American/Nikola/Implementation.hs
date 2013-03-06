{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module American.Nikola.Implementation (
    finalPut,
    prevPut,
    first
  ) where

import Prelude hiding (drop, init, map, max, min, tail, take, zipWith)

import Data.Int

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Eval.Target
import Data.Array.Nikola.Eval.Load
import Data.Array.Nikola.Array

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
bankDays = 256
s0 = 100
r = 0.03; alpha = 0.07; sigma = 0.20

finalPut :: Vector G (Exp F)
         -> Vector G (Exp F)
         -> Vector D (Exp F)
finalPut uPow dPow = pmax (strike -^ st) 0
  where
    st :: Vector D (Exp F)
    st = s0 *^ (uPow ^*^ dPow)

prevPut :: Vector G (Exp F)
        -> Vector G (Exp F)
        -> Exp Int32
        -> Vector G (Exp F)
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


type PutArr = (MArray G (Z :. Exp Ix) (Exp F))

first :: Int32 -> Vector G (Exp F) -> Vector G (Exp F) -> Exp Int32 -> P (Array D Z (Exp F))
first i uPow dPow expiry = do
  -- Allocate target array:
  mArr1 <- newMArray (Z :. n) :: P PutArr
  mArr2 <- newMArray (Z :. n) :: P PutArr
  loadP (finalPut uPow dPow) mArr1
  -- generate nikola code inside monad P
  (lastRarr,_) <- foldl (\act i -> do
      (rArr :: PutArr , wArr :: PutArr ) <- act
      rArr' <- unsafeFreezeMArray rArr
      loadP (prevPut uPow dPow expiry rArr' (lift i :: Exp Int32)) (wArr :: PutArr)
      return (wArr,rArr) -- Swap r/w-arrays.
    ) (return (mArr1, mArr2))
      [fromIntegral i, fromIntegral i-1 .. 1]

  firstArr <- unsafeFreezeMArray lastRarr
  -- return (index firstArr (Z :. 0) )
  return (fromFunction Z (\z -> index firstArr (Z :. 0)))

  where
    n = expiry * bankDays

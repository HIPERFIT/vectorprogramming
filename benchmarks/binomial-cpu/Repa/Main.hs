{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.List (foldl')
import Data.Array.Repa hiding ((-^), (*^))
import Data.Array.Repa.Eval

import Prelude hiding (map, zipWith)

import Control.Monad.Identity (runIdentity)
import System.IO

type F = Double

-- Pointwise manipulation of vectors and scalars
{-# INLINE (^*^) #-}
v1 ^*^ v2 = zipWith (*) v1 v2
{-# INLINE (^+^) #-}
v1 ^+^ v2 = zipWith (+) v1 v2
{-# INLINE (-^) #-}
c -^ v = map (c -) v
{-# INLINE (*^) #-}
c *^ v = map (c *) v

pmax v c = map (max c) v
ppmax = zipWith max

force :: (Target r2 e,
          Load r1 sh e) => Int -> Array r1 sh e -> Array r2 sh e
--force arr = arr `deepSeqArray` (suspendedComputeP arr)
--force = runIdentity . computeP
force i | i < 1000 = computeS
        | otherwise = computeP'

computeP' arr = arr `deepSeqArray` (suspendedComputeP arr)

binom :: Int -> F
binom expiry = repa_head first
  where
    uPow, dPow :: Array U DIM1 F
    uPow = force n $ fromFunction (Z :. n+1) (\(Z:.i) -> u^i)
    dPow = force n $ fromFunction (Z :. n+1) (\(Z:.i) -> d^(n-i))

    st :: Array D DIM1 F
    st = s0 *^ (uPow ^*^ dPow)
    finalPut = force n $ pmax (strike -^ st) 0

    first = foldl' prevPut finalPut [n, n-1 .. 1]

    {-# INLINE prevPut #-}
    prevPut :: Array U DIM1 F -> Int -> Array U DIM1 F
    prevPut put i = force i $ ppmax(strike -^ st) ((qUR *^ repa_tail put) ^+^ (qDR *^ repa_init put))
      where st = s0 *^ ((repa_take i uPow) ^*^ (repa_drop (n+1-i) dPow))

    -- standard econ parameters
    strike = 100
    bankDays = 256
    s0 = 100
    r = 0.03; alpha = 0.07; sigma = 0.20
    n = expiry*bankDays

    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

{-# INLINE repa_take #-}
repa_take i = extract (Z :. 0) (Z :. i)
{-# INLINE repa_drop #-}
repa_drop i arr = extract (Z :. i) (Z :. len - i) arr
  where (Z :. len) = extent arr
{-# INLINE repa_init #-}
repa_init arr = extract (Z :. 0) (Z :. len - 1) arr
  where (Z :. len) = extent arr
{-# INLINE repa_tail #-}
repa_tail arr = extract (Z :. 1) (Z :. len - 1) arr
  where (Z :. len) = extent arr
{-# INLINE repa_head #-}
repa_head arr = arr `unsafeIndex` (Z :. 0)


main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute binom

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " Prelude.++ (take 150 . show . f . read $ str)


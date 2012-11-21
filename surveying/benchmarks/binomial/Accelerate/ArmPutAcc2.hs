
module Main
  where


import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.List

import qualified Data.Array.Accelerate.Interpreter      as AI
import qualified Data.Array.Accelerate.CUDA             as ACUDA

-- Pointwise manipulation of vectors an scalars

infixl 7 ^*^
infixl 6 ^+^
(^*^), (^+^) :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a)
v1 ^*^ v2 = A.zipWith (*) v1 v2
v1 ^+^ v2 = A.zipWith (+) v1 v2

infixl 7 *^
infixl 6 -^
(-^), (*^) :: (Elt a, IsNum a) => Exp a -> Acc (Vector a) -> Acc (Vector a)
c -^ v = A.map (c -) v
c *^ v = A.map (c *) v

pmax :: (Elt a, IsScalar a) => Acc (Vector a) -> Exp a -> Acc (Vector a)
pmax v c = A.map (A.max c) v

ppmax :: (Elt a, IsScalar a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a)
ppmax = A.zipWith A.max


type FloatRep = Float


binom :: Int -> Acc (Vector FloatRep)
binom expiry = first
  where
    -- Actually don't want to share these definitions: they are relatively
    -- cheap so by parameterising by 'i' we effectively "force inline" into each
    -- put step.
    --
    -- Furthermore, this improves caching of the generated CUDA code kernels.
    --
    uPow, dPow :: Int -> Acc (Vector FloatRep)
    dPow i = A.drop (A.constant (n+1-i))
           $ A.reverse
           $ A.generate (A.constant (Z:.n+1)) (\ix -> d' ** A.fromIntegral (A.unindex1 ix))

    uPow i = A.take (A.constant i)
           $ A.generate (A.constant (Z:.n+1)) (\ix -> u' ** A.fromIntegral (A.unindex1 ix))


    -- The (>->) here instead of (.) further forces us to not share the
    -- definitions in uPow and dPow when sequencing the puts.
    --
    first = foldl1' (>->) (P.map prevPut [n, n-1 .. 1]) finalPut

    prevPut :: Int -> Acc (Vector FloatRep) -> Acc (Vector FloatRep)
    prevPut i prev = ppmax (strike -^ st) (qUR *^ A.tail prev ^+^ qDR *^ A.init prev)
      where
        st = s0 *^ (uPow i ^*^ dPow i)

    finalPut = pmax (strike -^ st) 0
      where
        st = s0 *^ (uPow (n+1) ^*^ dPow (n+1))

    -- standard econ parameters
    strike      = 100
    bankDays    = 5 --252
    s0          = 100
    r           = 0.03
    alpha       = 0.07
    sigma       = 0.20  :: FloatRep

    n           = expiry*bankDays
    dt          = P.fromIntegral expiry / P.fromIntegral n
    u           = exp(alpha*dt+sigma*sqrt dt)
    d           = exp(alpha*dt-sigma*sqrt dt)
    stepR       = exp(r*dt)
    q           = (stepR-d)/(u-d)
    qUR         = A.constant $ q/stepR
    qDR         = A.constant $ (1-q)/stepR

    u'          = A.constant u
    d'          = A.constant d


main = print
     . head
     . A.toList
     $ ACUDA.run (binom 1)


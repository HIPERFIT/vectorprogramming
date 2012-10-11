{-# LANGUAGE ScopedTypeVariables #-}
module American (binom, FloatRep) where
import qualified Data.Array.Accelerate as A
import Data.List(foldl')

-- Pointwise manipulation of vectors an scalars
v1 ^*^ v2 = A.zipWith (*) v1 v2
v1 ^+^ v2 = A.zipWith (+) v1 v2
c -^ v = A.map (c -) v
c *^ v = A.map (c *) v

pmax v c = A.map (A.max c) v
ppmax = A.zipWith A.max

vtail = A.tail
vinit = A.init
vtake i v = A.take (A.constant i) v
vdrop i v = A.drop (A.constant i) v

type FloatRep = Float
--type FloatRep = Double -- I would like to use Double, but then I get a large numbers of of ptxas errors like:
                          -- ptxas /tmp/tmpxft_00006c13_00000000-2_dragon26988.ptx, line 83; warning : Double is not supported. Demoting to float
                          -- and I just compute NaN


binom :: Int -> A.Acc(A.Vector FloatRep)
binom expiry = first --(first ! (A.constant 0))
  where
    uPow, dPow :: A.Acc(A.Vector FloatRep)
    uPow = generateWithPow (A.index1 . A.constant $ n+1) (A.constant u)
    dPow = A.reverse $ generateWithPow (A.index1 . A.constant $ n+1) (A.constant d)
    
    generateWithPow :: A.Exp A.DIM1 -> A.Exp FloatRep -> A.Acc (A.Vector FloatRep)
    generateWithPow m x = A.generate m $ \ix -> let i = A.unindex1 ix in x** (A.fromIntegral i)

    st = s0 *^ (uPow ^*^ dPow)
    finalPut = pmax (strike -^ st) 0

-- for (i in n:1) {
-- St<-S0*u.pow[1:i]*d.pow[i:1]
-- put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }
    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut :: A.Acc(A.Vector FloatRep) -> Int -> A.Acc(A.Vector FloatRep)
    prevPut put i =
      ppmax(strike -^ st) ((qUR *^ vtail put) ^+^ (qDR *^ vinit put))
        where st = s0 *^ ((vtake i uPow) ^*^ (vdrop (n+1-i) dPow))

    -- standard econ parameters
    strike = 100
    bankDays :: Int
    bankDays = 252
    s0 = 100
    r = 0.03; alpha = 0.07; sigma = 0.20

    n :: Int
    n = expiry*bankDays

    dt, u, d :: FloatRep
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = A.constant$ q/stepR; qDR = A.constant$ (1-q)/stepR

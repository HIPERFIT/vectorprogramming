{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when, forever, foldM)
import System.Exit (exitSuccess)
import System.IO
import Data.List (foldl')
import Prelude hiding (map, zipWith)

import Data.Vector.Unboxed (Unbox)
import Data.Array.Repa hiding ((-^), (*^))
import Data.Array.Repa.Eval




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

force :: (Unbox e, Load r1 sh e, Monad m) => Int -> Array r1 sh e -> m (Array U sh e)
force i | i < 10000 = return . computeS
        | otherwise = computeP

binom :: Monad m => Int -> m F
binom expiry = do
    uPow <- force n $ fromFunction (Z :. n+1) (\(Z:.i) -> u^i)
    dPow <- force n $ fromFunction (Z :. n+1) (\(Z:.i) -> d^(n-i))
    let st = s0 *^ (uPow ^*^ dPow)
    finalPut <- force n $ pmax (strike -^ st) 0
    lastIter <- foldM (prevPut uPow dPow) finalPut [n, n-1 .. 1]
    return $ repaHead lastIter
  where
    
    {-# INLINE prevPut #-}
    prevPut :: Monad m => Array U DIM1 F -> Array U DIM1 F -> Array U DIM1 F -> Int -> m (Array U DIM1 F)
    prevPut uPow dPow put i = force i $ ppmax(strike -^ st) ((qUR *^ repaTail put) ^+^ (qDR *^ repaInit put))
      where st = s0 *^ (repaTake i uPow ^*^ repaDrop (n+1-i) dPow)

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

{-# INLINE repaTake #-}
repaTake i = extract (Z :. 0) (Z :. i)
{-# INLINE repaDrop #-}
repaDrop i arr = extract (Z :. i) (Z :. len - i) arr
  where (Z :. len) = extent arr
{-# INLINE repaInit #-}
repaInit arr = extract (Z :. 0) (Z :. len - 1) arr
  where (Z :. len) = extent arr
{-# INLINE repaTail #-}
repaTail arr = extract (Z :. 1) (Z :. len - 1) arr
  where (Z :. len) = extent arr
{-# INLINE repaHead #-}
repaHead arr = arr `unsafeIndex` (Z :. 0)


main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute binom

execute :: (Read a, Show b) => (a -> IO b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  result <- f . read $ str
  putStrLn $ "RESULT " Prelude.++ take 150 (show result)


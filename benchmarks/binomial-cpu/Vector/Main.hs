module Main where
import qualified Data.Vector.Unboxed as V
import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.List(foldl')
import System.Environment(getArgs)
import System.IO

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

    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut put i = ppmax (strike -^ st) ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      where st = s0 *^ (V.take i uPow ^*^ V.drop (n+1-i) dPow)

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
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

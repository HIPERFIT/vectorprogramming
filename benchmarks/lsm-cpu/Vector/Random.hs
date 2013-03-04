module Random (boxMuller, boxMullerVector, normalsBoxMuller) where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UB
import qualified Data.Vector.Unboxed.Mutable as UBM

import Data.Vector.Random.Mersenne
import System.Random.Mersenne.Pure64


normals :: (UB.Unbox a, Floating a, PureMTRandom a) => PureMT -> Int -> Vector a
normals gen count = UB.map morosInversion $ randoms gen count

normalsBoxMuller :: (UB.Unbox a, Floating a, PureMTRandom a) => PureMT -> Int -> Vector a
normalsBoxMuller gen count = boxMullerVector $ randoms gen count


{-# INLINE boxMuller #-}
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

-- Only call with vectors of even length
boxMullerVector :: UB.Unbox a => Floating a => Vector a -> Vector a
boxMullerVector vec = UB.create $ do
  v <- UBM.new n
  boxMullerVec v 0
  return v
 where
   n = UB.length vec
   boxMullerVec v i | i >= n     = return ()
   boxMullerVec v i | otherwise = do
     let (a,b) = boxMuller (vec UB.! i) (vec UB.! (i + 1))
     UBM.write v i a
     UBM.write v (i+1) b
     boxMullerVec v (i+2)

-- | Convert a list of uniformly distributed random values into a
-- list of normally distributed random values. The Box-Muller
-- algorithms converts values two at a time, so if the input list
-- has an uneven number of element the last one will be discarded.
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
boxMullers _          = []

boxMullerVector2 :: UB.Unbox a => Floating a => Vector a -> Vector a
boxMullerVector2 = UB.fromList . boxMullers . UB.toList

-- ^ Convert a (0,1] uniformly distributed number to a normal
-- distributed value using Moro's inversion.
--
-- See "Monte Carlo Methods in Financial Engineering" by Paul
-- Glasserman, page 67.
{-# INLINE morosInversion #-}
morosInversion :: Floating a => a -> a
morosInversion u | 0.50 <= u && u <= 0.92 = beasleySpringerApprox
                 | 0.92  < u && u  < 1.00 = morosApprox
                 | otherwise              = - morosInversion (1-u)
  where
    beasleySpringerApprox = numerator / (1 + divisor)
      where
        ux = u - 0.5
        numerator = UB.foldl f (0,ux) as
        divisor   = UB.foldl f (0,1) bs
        f (s, m) a = (s + a * m, m * ux * ux)
    
    morosApprox = UB.foldl (\(s,m) c -> (s + c*m, m*logloginvu)) (0,1) cs
     where
       logloginvu = log(-log(1-u))

-- Contants used for morosInversion
as = UB.fromList [ 2.50662823884, -18.61500062529, 
                  41.39119773534, -25.44106049637]

bs = UB.fromList [-8.47351093090,  23.08336743743,
                  -21.06224101826,  3.13082909833]

cs = UB.fromList [0.3374754822726147, 0.9761690190917186,
                  0.1607979714918209, 0.0276438810333863,
                  0.0038405729373609, 0.0003951896511919,
                  0.0000321767881768, 0.0000002888167364,
                  0.0000003960315187]

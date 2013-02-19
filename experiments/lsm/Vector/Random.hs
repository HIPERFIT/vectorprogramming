module Random where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UB
import qualified Data.Vector.Unboxed.Mutable as UBM

import Data.Vector.Random.Mersenne
import System.Random.Mersenne.Pure64


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

normals :: (UB.Unbox a, Floating a, PureMTRandom a) => PureMT -> Int -> Vector a
normals gen count = boxMullerVector $ randoms gen count
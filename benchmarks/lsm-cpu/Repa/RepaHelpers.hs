{-# LANGUAGE FlexibleContexts #-}
module RepaHelpers where

import Data.Array.Repa

import Prelude hiding (zipWith)
import qualified Data.Vector.Unboxed as UB

empty :: Array D DIM1 e
empty = fromFunction (Z :. 0) undefined

singleton :: e -> Array D DIM1 e
singleton x = fromFunction (Z :. 1) (\_ -> x)

zip3 :: Shape sh => Array D sh e -> Array D sh e -> Array D sh e -> Array D sh (e,e,e)
zip3 as bs cs = zipWith (\a (b,c) -> (a,b,c)) as $ zipWith (,) bs cs

take :: Int -> Array D DIM1 e -> Array D DIM1 e
take n arr = if n > m then error "take: Index out of bounds"
             else backpermute (Z :. n) id arr
  where
   (Z :. m) = extent arr

reverse :: (Source r e ) => Array r DIM1 e -> Array D DIM1 e
reverse arr = backpermute (Z :. n) (\(Z :. i) -> Z :. n - i - 1) arr
  where
    (Z :. n) = extent arr


or :: (Shape s, Source r Bool) => Array r s Bool -> Bool
or = foldAllS (||) False

null :: Array D DIM2 Double -> Bool
null a = i*j == 0
  where
    (Z :. i :. j) = extent a


unindex1 :: DIM1 -> Int
unindex1 (Z :. e) = e

index1 :: Int -> DIM1
index1 e = Z :. e

length :: Source r e => Array r DIM1 e -> Int
length arr = let (Z :. n) = extent arr in n


zipWith3 :: (Shape sh, Source r1 a, Source r2 b, Source r3 c)
        => (a -> b -> c -> d)
        -> Array r1 sh a -> Array r2 sh b -> Array r3 sh c
        -> Array D sh d
zipWith3 f arr1 arr2 arr3
 = let  get ix  = f (arr1 `unsafeIndex` ix) (arr2 `unsafeIndex` ix) (arr3 `unsafeIndex` ix)
        {-# INLINE get #-}
        
   in   fromFunction 
                (intersectDim (extent arr1) (intersectDim (extent arr2) (extent arr3)))
                get



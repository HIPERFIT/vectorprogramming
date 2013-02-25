module LinAlg where

import Control.Monad.ST

import qualified Data.Vector as B
import Data.Vector.Unboxed hiding ((++))
import qualified Data.Vector.Unboxed.Mutable as MU
 
import Prelude hiding (sum, zipWith, zipWith3, length, map, foldl, reverse, null, replicate, tail, head, zip3, take, or, foldr)
import qualified Prelude
import Debug.Trace

polyvals p = map (polyval p)

{-# INLINE polyval #-}
polyval :: Vector Double -> Double -> Double
polyval ps x = foldl (\c0 p -> p + x*c0) 0.0 $ reverse ps

-- http://facstaff.unca.edu/mcmcclur/class/LinearII/presentations/html/leastsquares.html
vander :: Vector Double -> Int -> B.Vector (Vector Double)
vander xs degree = B.map (\x -> generate (degree + 1) (\i -> x ** (fromIntegral i))) (convert xs)


-- https://github.com/numpy/numpy/blob/master/numpy/lib/polynomial.py#L394

-- * Create Vandermonde matrix A
-- * Solve system A*c = y for c
--  - This can be done by LU, Cholesky or QR decomposition
polyfit :: Vector Double -> Vector Double -> Int -> Vector Double
polyfit xs ys degree = c --zipWith (/) c scale
  where
    a = vander xs degree
--    scale = map (sqrt . sum . (map (\x -> x*x))) $ transpose a
--    lhs = transpose $ zipWith (\as s -> map (/s) as) (transpose a) scale
--    c = lstsq_cholesky a ys 
    c = lstsq_cholesky a ys --solveWithHMatrix (matProd (transpose lhs) lhs) ((transpose lhs) `matVecProd` ys)

lstsq_cholesky :: B.Vector (Vector Double) -> Vector Double -> Vector Double
lstsq_cholesky a b = x
  where
    aT = transpose a
    c = matTransProd aT
    d = aT `matVecProd` b
    g = cholesky c
    y = forwardSubstitute g d
    x = backwardSubstitute (transpose g) y

dim :: Unbox a => B.Vector (Vector a) -> String
dim a = show n ++ "x" ++ show m
  where
    n = B.length a
    m = length (a B.! 0)

nullMatrix :: B.Vector (Vector Double) -> Bool
nullMatrix a = B.null a || B.or (B.map null a)

-- Return the (i,j) element of the lower triangular matrix.  (We assume the
-- lower array bound is (0,0).)
get :: B.Vector (Vector Double) -> B.Vector (Vector Double) -> (Int, Int) -> Double
get a l (i,j) | i == j = sqrt $ (a B.! j ! j) - dot
              | i  > j = ((a B.! i ! j) - dot) / (l B.! j ! j)
              | otherwise = 0
  where dot = Prelude.sum [(l B.! i ! k) * (l B.! j ! k) | k <- [0..j-1]]

cholesky :: B.Vector (Vector Double) -> B.Vector (Vector Double)
cholesky a = runST $ do
    l <- B.replicateM n (MU.new n)
    Prelude.mapM_ (update l) [(i,j) | i <- [0..n-1], j <- [0..n-1]]
    B.mapM unsafeFreeze l
  where
    n = B.length a
    update l (i,j) = B.mapM unsafeFreeze l >>= \l' -> MU.write (l B.! i) j (get a l' (i,j))

dotProd :: Vector Double -> Vector Double -> Double
dotProd xs ys | length xs /= length ys = error "Unequal lengths in dotProd"
              | otherwise = sum $ zipWith (*) xs ys

matProd :: B.Vector (Vector Double) -> B.Vector (Vector Double) -> B.Vector (Vector Double)
matProd xs ys = B.map (\x -> convert $ B.map (\y -> dotProd x y) (transpose ys)) xs

-- This takes a matrix A and performs the multiplication (A x transpose A)
-- We use it to avoid double transpositions
matTransProd :: B.Vector (Vector Double) -> B.Vector (Vector Double)
matTransProd xs = B.map (\x -> convert $ B.map (\y -> dotProd x y) xs) xs

matVecProd :: B.Vector (Vector Double) -> Vector Double -> Vector Double
matVecProd xs ys = convert $ B.map (dotProd ys) xs

{-# INLINE transpose #-}
transpose :: B.Vector (Vector Double) -> B.Vector (Vector Double)
transpose xs | nullMatrix xs = B.empty
             | otherwise  = B.cons (convert $ B.map head xs) $ transpose (B.map tail xs)

forwardSubstitute :: B.Vector (Vector Double) -> Vector Double -> Vector Double
forwardSubstitute l b = B.foldl subst empty (B.zip3 (B.generate (length b) id) l (convert b))
  where subst as (m, ls_m, b_m) = snoc as $ (b_m - dotProd as ls_m') / l_mm
          where
            ls_m' = take m ls_m
            l_mm :: Double
            l_mm = ls_m ! m

backwardSubstitute :: B.Vector (Vector Double) -> Vector Double -> Vector Double
backwardSubstitute u b = reverse $ forwardSubstitute ru rb
  where rb = (reverse b)
        ru = (B.reverse $ B.map reverse u)

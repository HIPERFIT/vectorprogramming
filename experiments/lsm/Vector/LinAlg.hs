module LinAlg (polyfit, polyval, polyvals) where

import Data.Vector hiding ((++))
import Prelude hiding (sum, zipWith, zipWith3, length, map, foldl, reverse, null, replicate, tail, head, zip3, take, or)
import qualified Prelude
import Debug.Trace
--import qualified Data.Packed.Matrix as HM
--import qualified Numeric.LinearAlgebra.Algorithms as HM

polyvals p = map (polyval p)

polyval :: Vector Double -> Double -> Double
polyval p x = sum $ zipWith (\p' n' -> p'*x**(fromIntegral n')) p pows
  where
   n = length p
   pows = reverse $ fromList [0..n-1]

-- http://facstaff.unca.edu/mcmcclur/class/LinearII/presentations/html/leastsquares.html
vander :: Vector Double -> Int -> Vector (Vector Double)
vander xs degree = transpose $ generate (degree + 1) (\x -> map (** (fromIntegral x)) xs)

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

--mdim a = (HM.rows a, HM.cols a)
    
-- lstsq_HMatrix :: Vector (Vector Double) -> Vector Double -> Vector Double
-- lstsq_HMatrix a b = x -- traceShow c x
--   where
--     aT = transpose a
--     c = aT `matProd` a
--     d = aT `matVecProd` b
--     l = cholesky c
--     y = forwardSubstitute l d
--     x = backwardSubstitute (transpose l) y

-- cholesky_HMatrix :: Vector (Vector Double) -> Vector (Vector Double)
-- cholesky_HMatrix a = map fromList . fromList . HM.toLists $ HM.chol amat
--   where
--     amat = HM.fromLists . toList $ map toList a

lstsq_cholesky :: Vector (Vector Double) -> Vector Double -> Vector Double
lstsq_cholesky a b = x
  where
    aT = transpose a
    c = aT `matProd` a
    d = aT `matVecProd` b
    l = cholesky c
    y = forwardSubstitute l d
    x = backwardSubstitute (transpose l) y

dim a = show n ++ "x" ++ show m
  where
    n = length a
    m = length (a ! 0)

nullMatrix :: Vector (Vector Double) -> Bool
nullMatrix a = null a || or (map null a)

cholesky :: Vector (Vector Double) -> Vector (Vector Double)
cholesky a | nullMatrix a = empty
cholesky a = merge l11 l21 l22
  where
    l11 = sqrt a11
    l21 = map (\x -> x / l11) a21
    ab = vecMult l21 l21
    a22' = zipWith (\x ys -> map (\y -> y-x) ys) ab a22
    l22 = cholesky a22'
    (a11, a21, a22) = split a
    
    vecMult :: Vector Double -> Vector Double -> Vector Double
    vecMult xs ys = zipWith (*) xs ys

    split :: Vector (Vector Double) -> (Double, Vector Double, Vector (Vector Double))
    split a | nullMatrix a = error "Should not happen"
    split a = (a11, a21, a22)
      where a11 = (a ! 0) ! 0
            a21 = map head (tail a)
            a22 = map tail (tail a)

    merge :: Double -> Vector Double -> Vector (Vector Double) -> Vector (Vector Double)
    merge l11 l21 l22 | nullMatrix l22 = singleton (singleton l11)
    merge l11 l21 l22 = cons row0 rest
      where
       row0 = cons l11 $ replicate (length (l22 ! 0)) 0.0
       rest = zipWith cons l21 l22
       
dotProd :: Vector Double -> Vector Double -> Double
dotProd xs ys | length xs /= length ys = error "Unequal lengths in dotProd"
              | otherwise = sum $ zipWith (*) xs ys

matProd :: Vector (Vector Double) -> Vector (Vector Double) -> Vector (Vector Double)
matProd xs ys = map (\x -> map (\y -> dotProd x y) (transpose ys)) xs

matVecProd :: Vector (Vector Double) -> Vector Double -> Vector Double
matVecProd xs ys = map (dotProd ys) xs

transpose :: Vector (Vector Double) -> Vector (Vector Double)
transpose xs | nullMatrix xs = empty
             | otherwise  = cons (map head xs) $ transpose (map tail xs)

forwardSubstitute l b = foldl subst empty (zip3 (generate (length b) id) l b)
  where subst as (m, ls_m, b_m) = snoc as $ (b_m - dotProd as ls_m') / l_mm
          where
            ls_m' = take m ls_m
            l_mm :: Double
            l_mm = ls_m ! m

backwardSubstitute u b = reverse $ forwardSubstitute ru rb
  where rb = (reverse b)
        ru = (reverse $ map reverse u)

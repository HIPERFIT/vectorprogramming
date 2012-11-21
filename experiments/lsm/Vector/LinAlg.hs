module LinAlg (polyfit, polyval) where

import Data.Vector hiding ((++))
import Prelude hiding (sum, zipWith, zipWith3, length, map, foldl, reverse, null, replicate, tail, head, zip3, take)
import qualified Prelude
import Debug.Trace
import qualified Data.Packed.Matrix as HM
import qualified Numeric.LinearAlgebra.Algorithms as HM

polyval :: Vector Double -> Vector Double -> Double
polyval p xs = sum $ zipWith3 (\p' n' x' -> p'*x'**(fromIntegral n')) p pows xs
  where
   n = length p
   pows = fromList [n-1..0]

-- http://facstaff.unca.edu/mcmcclur/class/LinearII/presentations/html/leastsquares.html
vander :: Vector Double -> Int -> Vector (Vector Double)
vander xs degree = transpose $ generate degree (\x -> map (** (fromIntegral x)) xs)

-- https://github.com/numpy/numpy/blob/master/numpy/lib/polynomial.py#L394

-- * Create Vandermonde matrix A
-- * Solve system A*c = y for c
--  - This can be done by LU, Cholesky or QR decomposition
polyfit :: Vector Double -> Vector Double -> Int -> Vector Double
polyfit xs ys degree = zipWith (/) c scale
  where
    a = vander xs (degree + 1)
    scale = map (sqrt . sum . (map (\x -> x*x))) $ transpose a
    lhs = transpose $ zipWith (\as s -> map (/s) as) (transpose a) scale
    c = solveWithHMatrix lhs ys -- solveWithHMatrix (matProd (transpose lhs) lhs) ((transpose lhs) `matVecProd` ys)

mdim a = (HM.rows a, HM.cols a)

solveWithHMatrix :: Vector (Vector Double) -> Vector Double -> Vector Double
solveWithHMatrix a b = fromList . Prelude.map Prelude.head . HM.toLists $ HM.linearSolveLS amat bmat
  where
    amat :: HM.Matrix Double
    amat = HM.fromLists . toList $ map toList a
    bmat :: HM.Matrix Double
--    bmat = HM.fromLists $ [toList b]
    bmat = HM.fromLists $ Prelude.map (:[]) (toList b)

solveWithCholesky :: Vector (Vector Double) -> Vector Double -> Vector Double
solveWithCholesky a b = x
  where
    l = cholesky a
    y = forwardSubstitute l b
    x = backwardSubstitute (transpose l) y

dim a = show n ++ "x" ++ show m
  where
    n = length a
    m = length (a ! 0)

cholesky :: Vector (Vector Double) -> Vector (Vector Double)
cholesky a | length a == 0 = empty
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
    split a | null a = error "Should not happen"
    split a = (a11, a21, a22)
      where a11 = (a ! 0) ! 0
            a21 = map head (tail a)
            a22 = map tail (tail a)

    merge :: Double -> Vector Double -> Vector (Vector Double) -> Vector (Vector Double)
    merge l11 l21 l22 | null l22 = singleton (singleton l11)
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
transpose xs | null xs || null (head xs) = empty
             | otherwise  = cons (map head xs) $ transpose (map tail xs)

forwardSubstitute l b = foldl subst empty (zip3 (generate (length b) id) l b)
  where subst as (m, ls_m, b_m) = snoc as $ (b_m - dotProd as ls_m) / l_mm
          where
            ls_m' = take m ls_m
            l_mm :: Double
            l_mm = ls_m ! m

backwardSubstitute u b = reverse $ forwardSubstitute ru rb
  where rb = (reverse b)
        ru = (reverse $ map reverse u)

-- ex_mat :: Vector (Vector Double)
-- ex_mat = fromList $ Prelude.map fromList [[3,-1,-1], [-1,3,-1], [-1,-1,3]]
-- ex_vec :: Vector Double
-- ex_vec = fromList [-651,2177,4069]

-- ex_mat2 :: Vector (Vector Double)
-- ex_mat2 = DV.fromList $ map DV.fromList [[1,0,0], [0,1,0], [2,-0.5,1]]
-- ex_vec2 :: Vector Double
-- ex_vec2 = DV.fromList [0,10,-11]

-- ex_mat3 :: Vector (Vector Double)
-- ex_mat3 = DV.fromList $ map DV.fromList [[-1,2,1], [0,8,6], [0,0,6]]



-- exm :: Vector (Vector Double)
-- exm = fromList $ Prelude.map fromList [[1, 2], [3,4]]
-- exv :: Vector Double
-- exv = fromList [5,6]

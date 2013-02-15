module LinAlg --(polyfit, polyval, polyvals) 
where
  
import Debug.Trace
import Control.Monad
import Control.Monad.ST

import Data.Array.Repa
import Data.Array.Repa.Eval
import Prelude hiding (sum, zipWith, zipWith3, length, map, foldl, reverse, null, replicate, tail, head, zip3, take, or, (++))
import qualified Prelude


import RepaHelpers  

polyvals :: Array D DIM1 Double -> Array D DIM1 Double -> Array D DIM1 Double
polyvals p = map (polyval p)

polyval :: Array D DIM1 Double -> Double -> Double
polyval p x = sumAllS $ zipWith (\p' n' -> p'*x**(fromIntegral n')) p pows
  where
   size = extent p
   n = unindex1 size
   pows = fromFunction size (\(Z :. i) -> n - i)

-- http://facstaff.unca.edu/mcmcclur/class/LinearII/presentations/html/leastsquares.html
vander :: Array D DIM1 Double -> Int -> Array D DIM2 Double
vander xs degree = fromFunction (Z :. n :. degree + 1) (\(Z :. i :. j) -> xs ! (Z :. i) ** (fromIntegral j))
  where
    n = unindex1 $ extent xs

-- * Create Vandermonde matrix A
-- * Solve system A*c = y for c
--  - This can be done by LU, Cholesky or QR decomposition
polyfit :: Array D DIM1 Double -> Array D DIM1 Double -> Int -> IO (Array D DIM1 Double)
polyfit xs ys degree = c --zipWith (/) c scale
  where
    a = vander xs degree
--    scale = map (sqrt . sum . (map (\x -> x*x))) $ transpose a
--    lhs = transpose $ zipWith (\as s -> map (/s) as) (transpose a) scale
--    c = lstsq_cholesky a ys 
    c = lstsq_cholesky a ys --solveWithHMatrix (matProd (transpose lhs) lhs) ((transpose lhs) `matVecProd` ys)

lstsq_cholesky :: Array D DIM2 Double -> Array D DIM1 Double -> IO (Array D DIM1 Double)
lstsq_cholesky a b = do 
  let aT = transpose a
  c <- aT `matProd` a
  d <- aT `matVecProd` b
  l <- cholesky (delay c)
  let y = forwardSubstitute (delay l) d
      x = backwardSubstitute (transpose l) y
  return x

dim :: Source r e => Array r DIM2 e -> String
dim a = show n Prelude.++ "x" Prelude.++ show m
  where
    (Z :. n :. m) = extent a

-- Cholesky decomposition ported from code on the Rosetta-code website
get :: Array D DIM2 Double -> Array U DIM2 Double -> (Int, Int) -> Double
get a l (i, j) | i == j = sqrt $ (a ! (Z :. j :. j)) - dot
               | i  > j = ((a ! (Z :. i :. j)) - dot) / (l ! (Z :. j :. j))
               | otherwise = 0
  where dot = Prelude.sum [(l ! (Z :. i :. k)) * (l ! (Z :. j :. k)) | k <- [0..j-1]]

cholesky :: Array D DIM2 Double -> IO (Array U DIM2 Double)
cholesky a = do
    l <- newMVec (n*n)
    mapM_ (update l) [(i,j) | i <- [0..n-1], j <- [0..n-1]]
    unsafeFreezeMVec (Z :. n :. n) l
  where
    (Z :. m :. n) = extent a
    update :: MVec U Double -> (Int, Int) -> IO ()
    update l (i,j) = do 
      l' <- unsafeFreezeMVec (Z :. n :. n) l
      unsafeWriteMVec l (i*n + j) (get a l' (i,j))


dotProd :: Array D DIM1 Double -> Array D DIM1 Double -> Double
dotProd xs ys | extent xs /= extent ys = error "Unequal lengths in dotProd"
              | otherwise = sumAllS $ zipWith (*) xs ys

dotProdUnsafe :: Array D DIM1 Double -> Array D DIM1 Double -> Double
dotProdUnsafe xs ys = sumAllS $ zipWith (*) xs ys


-- From the first repa paper
matProd  :: Monad m
         => Array D DIM2 Double
         -> Array D DIM2 Double
         -> m (Array U DIM2 Double)
matProd a b = sumP (zipWith (*) aRepl bRepl)
    where
      t     = transpose b
      aRepl = extend (Z :.All :.colsB :.All) a
      bRepl = extend (Z :.rowsA :.All :.All) t
      (Z :.colsA :.rowsA) = extent a
      (Z :.colsB :.rowsB) = extent b

matVecProd :: Monad m => Array D DIM2 Double -> Array D DIM1 Double -> m (Array D DIM1 Double)
matVecProd xs ys = do
  when (n /= n') $ error "matVecProd: Incompatible dimensions"
  x <- matProd xs (reshape (Z :. n :. 1) ys)
  return $ reshape (Z :. m) x
   where
     (Z :. n) = extent ys
     (Z :. n' :. m) = extent xs

-- Why does sequential repa-fold have type (a -> a -> a) for the folding function?

forwardSubstitute :: Array D DIM2 Double -> Array D DIM1 Double -> Array D DIM1 Double
forwardSubstitute l b = if n /= n' then error "Incompatible matrix and vector in forwardSubstitution"
                        else Prelude.foldl subst empty [0..n-1]
  where
    (Z :. n :. m) = extent l
    (Z :. n') = extent b
    subst :: Array D DIM1 Double -> Int -> Array D DIM1 Double
    subst y i = y ++ (singleton $ (b_i - dotProdUnsafe y l_i) / l_ii)
      where
        l_i :: Array D DIM1 Double
        l_i = slice l (Z :. i :. All)
        b_i :: Double
        b_i = b ! (Z :. i)
        l_ii :: Double
        l_ii = l_i ! (Z :. i)

-- Performs backwardSubstitution given a **LOWER** triangular matrix
-- (as the one from the above cholesky function)
-- That is we perform backward substitution on the transposed version
-- of the given matrix (to avoid double transposition)
backwardSubstitute :: Array D DIM2 Double -> Array D DIM1 Double -> Array D DIM1 Double
backwardSubstitute u b = reverse $ forwardSubstitute ru rb
  where
    rb :: Array D DIM1 Double
    rb = reverse b
    ru :: Array D DIM2 Double
    ru = transposeAlternative u

    -- transpose matrix through the other diagonal
    transposeAlternative :: Source r e => Array r DIM2 e -> Array D DIM2 e
    transposeAlternative arr = backpermute (Z :. n :. m) (\(Z :. i :. j) -> (Z :. m - j - 1 :. n - i - 1)) arr
      where (Z :. m :. n) = extent arr

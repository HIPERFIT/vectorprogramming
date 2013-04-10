{-# LANGUAGE ScopedTypeVariables #-}
module Examples where

import Data.Array.Accelerate as A

sobolND :: Acc (Array DIM2 Int) -> Exp Int -> Acc (Array DIM2 Double)
sobolND =  undefined -- Huge elaborate cube construction

-- Compute Euclidian distance between two N-dimensional points.
distND :: Acc (Array DIM1 Double) -> Acc (Array DIM1 Double) -> Exp Double
distND xs ys =
  let
    diffSq = A.zipWith (\y x -> (y - x) ^ 2) ys xs
  in sqrt $ the $ A.fold (+) 0.0 diffSq

-- Approximate π using a set of evenly distributed points in 2D plane interval [0;1].
pi :: Acc (Array DIM2 Double) -> Exp Double
pi nums =
  let
    n = 10 -- A.snd $ unindex2 $ shape nums
    -- We may even reuse distND!
    dists = generate (index1 n) (\ix ->
      distND 
          (use $ fromList (Z :. 2) [0,0]) 
          (use $ fromList (Z :. 2) [0,1]) -- replace line with 'slice below'
          -- it seems my slicing is off somehow.. Accelerate says: Cyclic definition???
          -- (slice nums (lift $ Z :. All :. (unindex1 ix)))
          )
  in the $ A.fold (\acc x -> acc + x {-(A.fromIntegral $ (A.truncate x :: Exp Int))-}) 0 dists

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

-- (Explodes - don't use)
-- Approximate π using a set of evenly distributed points in 2D plane interval [0;1].
pi :: Acc (Array DIM2 Double) -> Exp Double
pi nums =
  let
    n = A.snd $ unindex2 $ shape nums
    -- We may even reuse distND!
    dists = generate (index1 n) (\ix ->
      let ix' = unindex1 ix
      in distND 
          (use $ fromList (Z :. 2) [0,0]) 
          --(use $ fromList (Z :. 2) [0,1]) -- replace line with 'slice below'

          -- it seems my slicing is off somehow.. Accelerate says: Cyclic definition???
          -- both the 'slice' and 'generate' lines below seem to collapse once we use 'ix''
          (generate (index1 2) (\dim -> nums ! (lift $ dim :. ix')))
          -- (slice nums (lift $ Z :. All :. ix'))
          )
  in the $ A.fold (\acc x -> acc + x {-(A.fromIntegral $ (A.truncate x :: Exp Int))-}) 0 dists

pi2d :: Acc (Array DIM2 Double) -> {-Acc (Array DIM1 Double) -}  Exp Double
pi2d nums =
    let
      n = A.fst $ unindex2 $ shape nums
      dists = generate (index1 n) 
                (\ix -> let ix' = unindex1 ix
                        in sqrt $ (nums ! (lift $ Z :. ix' :. (0 :: Exp Int))) ^ 2
                                + (nums ! (lift $ Z :. ix' :. (1 :: Exp Int))) ^ 2)
  in 4 * ((A.fromIntegral n)-(the $ A.fold (\x acc -> acc + (A.fromIntegral $ (A.truncate x :: Exp Int))) 0 dists)) 
         / (A.fromIntegral n :: Exp Double)

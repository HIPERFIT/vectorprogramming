{-# LANGUAGE ScopedTypeVariables #-}
module Pi where

import Data.Array.Accelerate as A
import Data.Bits

-- hard-coded direction vectors , 32 x d
dirvs :: Acc (Array DIM2 Word32)
dirvs = generate (lift $ Z :. (1 :: Int) :. (30 :: Int))
          (\ix -> let i = A.snd $ unindex2 ix
                  in A.setBit 0 (29-i))

-- dirvs : d x 32 matrix ?
sobolND :: Acc (Array DIM2 Word32) -> Exp Int -> Acc (Array DIM2 Double)
sobolND dirVs n = let
  (nDirVs :: Exp Int, width :: Exp Int) = unlift $ unindex2 $ shape dirVs

  -- n x d x 32
  dirVsRep = A.replicate (lift $ Z :. n :. All :. All ) dirVs

  -- n x 32
  nBitVecs = generate (lift $ Z :. n :. width)
               (\ix -> let Z :. (i :: Exp Int) :. (bit :: Exp Int) = unlift ix
                       in A.fromIntegral . boolToInt $ A.testBit i bit :: Exp Word32)

  -- n x d x 32
  nBitRep  = A.replicate (lift $ Z :. All :. nDirVs :. All)  nBitVecs
    
  in A.map normalise
       -- n x d x 32 -> n x d
     $ A.fold1 (xor) (A.zipWith (*) dirVsRep nBitRep)

  where
    normalise :: Exp Word32 -> Exp Double
    normalise x = (A.fromIntegral x) / (A.fromIntegral (A.setBit 0 30 :: Exp Word32))

pi2d :: Acc (Array DIM2 Double) -> Exp Double
pi2d nums =
    let
      n = A.fst $ unindex2 $ shape nums
      dists = generate (index1 n) 
                (\ix -> let ix' = unindex1 ix
                        in sqrt $ (nums ! (lift $ Z :. ix' :. (0 :: Exp Int))) ^ 2
                                + (nums ! (lift $ Z :. ix' :. (1 :: Exp Int))) ^ 2)
  in 4 * ((A.fromIntegral n)-(the $ A.fold (\x acc -> acc + (A.fromIntegral $ (A.truncate x :: Exp Int))) 0 dists)) 
         / (A.fromIntegral n :: Exp Double)

{-
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
-}


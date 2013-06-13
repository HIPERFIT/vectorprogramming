{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pi where

import Data.Array.Accelerate
import qualified Prelude as P
import Prelude hiding (zipWith, map, fromIntegral, replicate, fst, snd, truncate)
import Data.Bits hiding (bit, testBit)

bitcount :: Int
bitcount = 32

dirvs :: Array DIM2 Int
dirvs = fromList (Z :. 2 :. bitcount)
              $ concat 
               [[2147483648,1073741824,2684354560,1342177280,
                 2281701376,603979776,301989888,754974720,
                 1988100096,2654994432,136314880,1678770176,
                 2988965888,2098462720,4272029696,3125346304,
                 438599680,1226522624,3300237312,3816001536,
                 4135585792,3728737280,2820672000,873465088,
                 975702144,1494483520,3970040096,2538144464,
                 1822721896,3613084132,3432358018,2271450689],

                [2147483648,1073741824,3758096384,2952790016,
                 2550136832,2483027968,2315255808,1526726656,
                 864026624,3653238784,1914699776,1058013184,
                 3250061312,2800484352,1401290752,703922176,
                 171606016,455786496,3549618176,1778348032,
                 3929540608,2871788544,1269173760,4259646208,
                 1610779008,4026976576,2016733344,605713840,
                 305826616,3475687836,3113412898,2197780721]
                 ]


normalise :: Exp Int -> Exp Float
normalise x = fromIntegral x / 2^bitcount

bitVector :: Exp Int -> Acc (Vector Int)
bitVector e = generate (index1 $ constant bitcount) gen
 where  gen = boolToInt . testBit e . unindex1

sobol :: Acc (Vector Int) -> Exp Int -> Exp Float
sobol v i = normalise (the xi)
 where
  xi = fold xor 0 $ zipWith (*) v (bitVector i)
  
-- sobol1D :: Exp Int -> Acc (Vector Int) -> Acc (Vector Float)
-- sobol1D m v = generate (index1 m) gen
--   where gen ix = sobol v (unindex1 ix + 1)

bitVectors2D :: Exp Int -> Acc (Array DIM2 Int)
bitVectors2D n = generate (index2 n $ constant bitcount) gen
 where gen ix = let Z :. e :. i = unlift ix
                in  boolToInt $ testBit e i

sobol1D :: Exp Int -> Acc (Vector Int) -> Acc (Vector Float)
sobol1D m vec = map normalise xi
 where
  xi = fold xor 0 $ zipWith (*) vecRep mat
  mat = bitVectors2D m
  vecRep = replicate (lift (Z :. m :. All)) vec


-- -- We would hope to write:
-- -- and it compiles, but results in:
-- -- *** Exception: Cyclic definition of a value of type 'Exp' (sa = 1875)
-- sobolND :: Acc (Scalar Int)
--         -> Acc (Scalar Int)
--         -> Acc (Array DIM2 Int)
--         -> Acc (Array DIM2 Float)
-- sobolND m n vs = generate (index2 (the m) (the n)) gen
--  where
--   gen :: Exp DIM2 -> Exp Float
--   gen ix = sobol v i
--     where
--      i :: Exp Int
--      j :: Exp Int
--      Z :. i :. j = unlift ix
--      v = slice vs (lift $ Z :. j :. All)

bitVectors :: Exp Int -> Exp Int
           -> Acc (Array DIM3 Int)
bitVectors n j = generate outSh gen
 where
  outSh = lift (Z :. n :. j :. constant bitcount)
  gen ix = fromIntegral . boolToInt $ testBit e i
   where Z :. e :. _ :. i = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int

sobolNDA :: Acc (Array DIM2 Int) -> Acc (Scalar Int) -> Acc (Array DIM2 Float)
sobolNDA vs n_arr = map normalise $ fold xor 0 $ zipWith (*) dirvs_rep (bitVectors n j)
  where
    j = fst . unindex2 . shape $ vs
    n = the n_arr
    dirvs_rep = replicate (lift $ Z :. n :. All :. All) vs

pi2d :: Acc (Array DIM2 Float) -> Acc (Scalar Float)
pi2d nums = unit $ 4 * ((fromIntegral $ n - (the $ fold1All (+) dists)) / (fromIntegral n))
 where
   n = fst . unindex2 . shape $ nums
   dists :: Acc (Array DIM1 Int)
   dists = generate (index1 n) 
              (\ix -> let ix' = unindex1 ix
                      in truncate $ sqrt $ (nums ! (lift $ Z :. ix' :. (0 :: Exp Int))) ^ (2 :: Int)
                                         + (nums ! (lift $ Z :. ix' :. (1 :: Exp Int))) ^ (2 :: Int))

runPi :: Acc (Scalar Int) -> Acc (Scalar Float)
runPi n = pi2d $ sobolNDA (use dirvs) n

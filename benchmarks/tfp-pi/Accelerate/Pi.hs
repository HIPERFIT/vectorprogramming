{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pi where

import Data.Array.Accelerate
import qualified Prelude as P
import Prelude hiding (zipWith, map, fromIntegral, replicate, fst, snd, truncate)
import Data.Bits hiding (bit, testBit)
import qualified Data.List as L

bitcount :: Int
bitcount = 32

dirvs :: Array DIM2 Word32
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

normalise :: Exp Word32 -> Exp Float
normalise x = fromIntegral x / 2^bitcount

bitVectors :: Exp Int -> Exp Int -> Acc (Array DIM3 Word32)
bitVectors n j = generate (lift $ Z :. n :. j :. constant bitcount) helper
  where
    helper ix = let Z :. e :. _ :. i = unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
                in fromIntegral . boolToInt $ testBit e i

sobolNDA :: Acc (Array DIM2 Word32) -> Exp Int -> Acc (Array DIM2 Float)
sobolNDA dirvs n = map normalise $ fold xor 0 $ zipWith (*) dirvs_rep (bitVectors n j)
  where
    j = fst . unindex2 . shape $ dirvs
    dirvs_rep = replicate (lift $ Z :. n :. All :. All) dirvs

pi2d :: Acc (Array DIM2 Float) -> Exp Float
pi2d nums = 4 * ((fromIntegral $ n - (the $ fold1All (+) dists)) / (fromIntegral n))
 where
   n = fst . unindex2 . shape $ nums
   dists :: Acc (Array DIM1 Int)
   dists = generate (index1 n) 
              (\ix -> let ix' = unindex1 ix
                      in truncate $ sqrt $ (nums ! (lift $ Z :. ix' :. (0 :: Exp Int))) ^ 2
                                         + (nums ! (lift $ Z :. ix' :. (1 :: Exp Int))) ^ 2)

runPi :: Exp Int -> Exp Float
runPi n = pi2d $ sobolNDA (use dirvs) n

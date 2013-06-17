module Sobol where

import Data.Bits (xor, testBit)
import Data.Word (Word32)

bitcount :: Num a => a
bitcount = 32

type Elem = Word32

dirvs :: [[Elem]]
dirvs = [[2147483648,1073741824,2684354560,1342177280,
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
          305826616,3475687836,3113412898,2197780721]]

normalise :: Elem -> Float
normalise x = fromIntegral x / 2^(bitcount :: Elem)

bitVector :: Elem -> [Elem]
bitVector i = map (fromIntegral . fromEnum . testBit i)
                  [0..bitcount - 1]

sobol :: [Elem] -> Elem -> Float
sobol v i = normalise xi
 where
  xi = foldl xor 0 $ zipWith (*) v (bitVector i)

sobol1D :: Elem -> [Elem] -> [Float]
sobol1D m v = map (sobol v) [1..m]

sobolND :: Elem -> [[Elem]] -> [[Float]]
sobolND m vs = map (sobol1D m) vs

pi2d :: [(Float, Float)] -> Float
pi2d xs = 4 * (n - fromIntegral (sum dists)) / n
 where
   n = fromIntegral $ length xs
   dist (x,y) = sqrt $ x^(2 :: Elem) + y^(2 :: Elem)
   dists :: [Elem]
   dists = map (truncate . dist) xs

computepi :: Elem -> Float
computepi n = pi2d $ zip xs ys
 where
  [xs, ys] = sobolND n dirvs

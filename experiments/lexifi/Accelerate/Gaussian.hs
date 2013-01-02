-- | given a random number, generate a gausian number
module Gaussian (gaussian) where

import Data.List
import Data.Array.Accelerate hiding ((!!), reverse, map)
import qualified Data.Array.Accelerate as Acc
import PricingTypesS

----------------------------
--- BEGIN Constant Lists ---
----------------------------

-- 1. For smallcase:
a_small ::[Exp SpecReal]
a_small = [       3.387132872796366608,
                  133.14166789178437745,
                  1971.5909503065514427,
                  13731.693765509461125,
                  45921.953931549871457,
                  67265.770927008700853,
                  33430.575583588128105,
                  2509.0809287301226727]
r_a_small = reverse a_small

b_small ::[Exp SpecReal]
b_small = [       1.0,
                  42.313330701600911252,
                  687.1870074920579083,
                  5394.1960214247511077,
                  21213.794301586595867,
                  39307.89580009271061,
                  28729.085735721942674,
                  5226.495278852854561]
r_b_small = reverse b_small

-- 2. For intermediate:
a_interm :: [Exp SpecReal]
a_interm = [      1.42343711074968357734,
                  4.6303378461565452959,
                  5.7694972214606914055,
                  3.64784832476320460504,
                  1.27045825245236838258,
                  0.24178072517745061177,
                  0.0227238449892691845833,
                  7.7454501427834140764e-4]
r_a_interm = reverse a_interm

b_interm ::[Exp SpecReal]
b_interm = [      1.0,
                  2.05319162663775882187,
                  1.6763848301838038494,
                  0.68976733498510000455,
                  0.14810397642748007459,
                  0.0151986665636164571966,
                  5.475938084995344946e-4,
                  1.05075007164441684324e-9]
r_b_interm = reverse b_interm

-- 3. For tail:
a_tail ::[Exp SpecReal]
a_tail   = [      6.6579046435011037772,
                  5.4637849111641143699,
                  1.7848265399172913358,
                  0.29656057182850489123,
                  0.026532189526576123093,
                  0.0012426609473880784386,
                  2.71155556874348757815e-5,
                  2.01033439929228813265e-7]
r_a_tail = reverse a_tail

b_tail ::[Exp SpecReal]
b_tail = [        1.0,
                  0.59983220655588793769,
                  0.13692988092273580531,
                  0.0148753612908506148525,
                  7.868691311456132591e-4,
                  1.8463183175100546818e-5,
                  1.4215117583164458887e-7,
                  2.04426310338993978564e-15]
r_b_tail = reverse b_tail


----------------------------
--- END   Constant Lists ---
----------------------------

-- strict variant for foldl is foldl'
-- tmp_rat_foldl :: RealFloat r => r -> [r] -> [r] -> r
tmp_rat_foldl :: Exp SpecReal -> [Exp SpecReal] -> [Exp SpecReal] -> Exp SpecReal
tmp_rat_foldl x as bs = let reduce xs = (foldl' (\b c -> x*b + c) 0 xs)
                        in reduce as / reduce bs

smallcasel :: Exp SpecReal -> Exp SpecReal
smallcasel q = q * tmp_rat_foldl (0.180625 - q*q) r_a_small r_b_small

tmp_intermediatel :: Exp SpecReal -> Exp SpecReal
tmp_intermediatel q = tmp_rat_foldl (q - 1.6) r_a_interm r_b_interm

tmp_taill ::  Exp SpecReal -> Exp SpecReal
tmp_taill q = tmp_rat_foldl (q - 5.0) r_a_tail r_b_tail

gaussianElem ::  Exp SpecReal -> Exp SpecReal
gaussianElem q =
        let dq = q - 0.5
        in ( abs dq <=* 0.425 ) ? (
               smallcasel dq -- tmp_small_case dq
           ,
               let pp = (dq <* 0.0) ? (q, 1.0 - q)
                   s  = sqrt (0.0 - log pp)
                   x  = (s <=* 5.0) ? (tmp_intermediatel s, tmp_taill s)
               in (dq <* 0.0) ? (-x, x))

gaussian :: Acc (Array DIM2 SpecReal) -> Acc (Array DIM2 SpecReal)
gaussian lst = Acc.map gaussianElem lst
-- can be replaced by another method do make a std.normal gaussian
-- distrib. from a uniform [0,1] distrib

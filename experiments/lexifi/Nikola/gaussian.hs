{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Main where

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Backend.Main
import Data.Array.Nikola.Backend.Flags
import Prelude hiding (drop, init, map, tail, take, zipWith, max, reverse)
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.List as DL
import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector.Storable as V
import System.Environment -- access to arguments etc.
import Text.PrettyPrint.Mainland
import System.IO(stdout)

type F = Double

--- BEGIN Constant Lists ---

-- 1. For smallcase:
a_small ::[Exp F]
a_small = [3.387132872796366608,
           133.14166789178437745,
           1971.5909503065514427,
           13731.693765509461125,
           45921.953931549871457,
           67265.770927008700853,
           33430.575583588128105,
           2509.0809287301226727]
r_a_small = DL.reverse a_small

b_small ::[Exp F]
b_small = [1.0,
           42.313330701600911252,
           687.1870074920579083,
           5394.1960214247511077,
           21213.794301586595867,
           39307.89580009271061,
           28729.085735721942674,
           5226.495278852854561]
r_b_small = DL.reverse b_small

-- 2. For intermediate:
a_interm :: [Exp F]
a_interm = [1.42343711074968357734,
            4.6303378461565452959,
            5.7694972214606914055,
            3.64784832476320460504,
            1.27045825245236838258,
            0.24178072517745061177,
            0.0227238449892691845833,
            7.7454501427834140764e-4]
r_a_interm = DL.reverse a_interm

b_interm ::[Exp F]
b_interm = [1.0,
            2.05319162663775882187,
            1.6763848301838038494,
            0.68976733498510000455,
            0.14810397642748007459,
            0.0151986665636164571966,
            5.475938084995344946e-4,
            1.05075007164441684324e-9]
r_b_interm = DL.reverse b_interm

-- 3. For tail:
a_tail ::[Exp F]
a_tail   = [6.6579046435011037772,
            5.4637849111641143699,
            1.7848265399172913358,
            0.29656057182850489123,
            0.026532189526576123093,
            0.0012426609473880784386,
            2.71155556874348757815e-5,
            2.01033439929228813265e-7]
r_a_tail = DL.reverse a_tail

b_tail ::[Exp F]
b_tail = [1.0,
          0.59983220655588793769,
          0.13692988092273580531,
          0.0148753612908506148525,
          7.868691311456132591e-4,
          1.8463183175100546818e-5,
          1.4215117583164458887e-7,
          2.04426310338993978564e-15]
r_b_tail = DL.reverse b_tail


----------------------------
--- END   Constant Lists ---
----------------------------

-- strict variant for foldl is foldl'
-- tmp_rat_foldl :: RealFloat r => r -> [r] -> [r] -> r
tmp_rat_foldl :: [Exp F] -> [Exp F] -> Exp F -> Exp F
tmp_rat_foldl as bs = vapply $ \x -> let
                          reduce xs = (DL.foldl' (\b c -> x*b + c) 0 xs)
                        in reduce as / reduce bs

smallcasel :: Exp F -> Exp F
smallcasel q = q * tmp_rat_foldl  r_a_small r_b_small (0.180625 - q*q)

tmp_intermediatel :: Exp F -> Exp F
tmp_intermediatel q = tmp_rat_foldl  r_a_interm r_b_interm (q - 1.6)

tmp_taill ::  Exp F -> Exp F
tmp_taill = vapply $ \q -> tmp_rat_foldl  r_a_tail r_b_tail (q - 5.0)

gaussianElem ::  Exp F -> Exp F
gaussianElem q =
        let dq = q - 0.5
        in if( abs dq <=* 0.425 ) then
               smallcasel dq
           else
               let pp = if dq <* 0.0 then q else (1.0 - q)
                   s  = sqrt (0.0 - (log pp))
                   x  = if (s <=* 5.0) then tmp_intermediatel s else tmp_taill s
               in if dq <* 0.0 then (-x) else x

gaussian :: Vector M (Exp F) -> Vector D (Exp F)
gaussian lst = map gaussianElem lst
-- can be replaced by another method do make a std.normal gaussian
-- distrib. from a uniform [0,1] distrib
--

{-
main = do
  let
      inputVec :: CV.Vector F
      inputVec = CV.fromHostVector $ V.fromList [1.0, 2.0, 10.0]
      gauss    :: CV.Vector F -> CV.Vector F
      gauss    = NH.compile gaussian
      res      = gauss inputVec
  pprIO defaultFlags stdout gaussian
  --print res
  -}

main = defaultMain gaussian

{-# LANGUAGE RecordWildCards #-}

module MonteCarloPricing where

import qualified Data.Bits as DB -- bit twiddling with shift and xor
import qualified Data.List as DL     -- HOFs for lists
import Control.Monad.ST
import Prelude hiding (reverse)

import Data.Array.Repa (Array, DIM1, DIM2, Z(..), Any(..), All(..), (:.)(..), U, D, Source, Shape)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Eval

import qualified Data.Vector as VB   -- vector, boxed
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU




import PricingTypesS

type Index = Integer
type F = Double


-- Repa helper functions
column :: Source r a => Array r DIM2 a -> Int -> Array D DIM2 a
column arr ix = 
  let arr' = R.slice arr (Any :. All :. ix)
      (Z :. n) = R.extent arr'
  in R.reshape (Z :. n :. 1) arr'

columns :: Source r a => Array r DIM2 a -> [Int] -> Array D DIM2 a
columns arr = foldl1 (R.++) . map (column arr)

reverse :: Source r a => Array r DIM1 a -> Array D DIM1 a
reverse arr =  let sh@(Z :. n) = R.extent arr
               in R.backpermute sh (\(Z :. i) -> Z :. (n-i)) arr


-----------------------------------------------
--- BEGIN SOBOL                             ---
-----------------------------------------------

lsb0_help ell c | (c DB..&. 1 == 0) = ell
                | otherwise = lsb0_help (ell+1) (c `DB.shiftR` 1)

-- PROBLEM: min{ k | (rep n)[k] == 0}
-- lsb0 :: Index -> Index
lsb0 n = lsb0_help 0 n

grayCode :: Index -> Elem
grayCode n = fromIntegral (n `DB.xor` (n `DB.shiftR` 1))


-- -- independent formula
-- -- LENGTH(RET) = LENGTH(sobol_dirVs)
sobolInd_  :: Pricing_Data -> Index -> Array U DIM1 Elem
sobolInd_ Pricing_Data{..} n = 
  let indices  = filter (DB.testBit (grayCode n)) [ 0..sobol_bit_count-1 ]
  in R.foldS DB.xor 0 $ columns sobol_dirVs indices

-- -- or compute by recurrence
-- -- INVAR: n >= 0 \implies sobolInd_ l (n+1) \equiv sobolRec_ l (n+1) (sobolInd_ l n)
-- sobolRec_ :: Pricing_Data -> [Elem] -> Index -> [Elem]
-- sobolRec_ Pricing_Data{..} prev n = zipWith DB.xor prev dirVs
--   where dirVs = [ vs VB.! bit | vs <- sobol_dirVs]
--         bit   = lsb0 n -- rmt n

sobolInd :: Pricing_Data -> Index -> Array D DIM1 F
sobolInd l n = R.map norm (sobolInd_ l n)
    where norm = ( / sobol_divisor l ) . fromIntegral



--mapArray :: (Array shA F -> Array shB F) -> Array shC F -> Array (shC-shA+shB)

--fromF :: shA -> (shA -> Array shB F) -> Array (shA :++: shB) F

mc_pricing :: Pricing_Data -> F -- output: one final price
mc_pricing l = let  zs :: [[[F]]] --Array D DIM2 F]
                    zs = Prelude.map
                         ( black_scholes l
                               . brownian_bridge_gen l
                               . gaussian
                               . sobolInd l) [1..num_iters l]
                    -- payoff = call_payoff 4000
               in mc_red l zs

mc_red :: Pricing_Data -> [[[F]]] -> F --[Array D DIM2 F] -> F
mc_red = undefined

----------------------------
--- BEGIN Constant Lists ---
----------------------------

-- 1. For smallcase:
a_small ::Array U DIM1 SpecReal
a_small = R.fromListUnboxed (Z :. 8)
                 [3.387132872796366608,
                  133.14166789178437745,
                  1971.5909503065514427,
                  13731.693765509461125,
                  45921.953931549871457,
                  67265.770927008700853,
                  33430.575583588128105,
                  2509.0809287301226727]
r_a_small = reverse a_small

b_small ::Array U DIM1 SpecReal
b_small = R.fromListUnboxed (Z :. 8)
                 [1.0,
                  42.313330701600911252,
                  687.1870074920579083,
                  5394.1960214247511077,
                  21213.794301586595867,
                  39307.89580009271061,
                  28729.085735721942674,
                  5226.495278852854561]
r_b_small = reverse b_small

-- 2. For intermediate:
a_interm :: Array U DIM1 SpecReal
a_interm = R.fromListUnboxed (Z :. 8)
                 [1.42343711074968357734,
                  4.6303378461565452959,
                  5.7694972214606914055,
                  3.64784832476320460504,
                  1.27045825245236838258,
                  0.24178072517745061177,
                  0.0227238449892691845833,
                  7.7454501427834140764e-4]
r_a_interm = reverse a_interm

b_interm ::Array U DIM1 SpecReal
b_interm = R.fromListUnboxed (Z :. 8)
                 [1.0,
                  2.05319162663775882187,
                  1.6763848301838038494,
                  0.68976733498510000455,
                  0.14810397642748007459,
                  0.0151986665636164571966,
                  5.475938084995344946e-4,
                  1.05075007164441684324e-9]
r_b_interm = reverse b_interm

-- 3. For tail:
a_tail ::Array U DIM1 SpecReal
a_tail   = R.fromListUnboxed (Z :. 8)
                 [6.6579046435011037772,
                  5.4637849111641143699,
                  1.7848265399172913358,
                  0.29656057182850489123,
                  0.026532189526576123093,
                  0.0012426609473880784386,
                  2.71155556874348757815e-5,
                  2.01033439929228813265e-7]
r_a_tail = reverse a_tail

b_tail ::Array U DIM1 SpecReal
b_tail = R.fromListUnboxed (Z :. 8)
                 [1.0,
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

ixVec :: Source r e => Array r DIM1 e -> Int -> e
ixVec arr i = R.index arr (Z :. i)

tmp_rat_evalL :: SpecReal -> Array U DIM1 SpecReal -> SpecReal
tmp_rat_evalL x a =
               (x*(x*(x*(x*(x*(x*(x*(a`ixVec`7) + a`ixVec`6)+a`ixVec`5)+a`ixVec`4)+a`ixVec`3)+a`ixVec`2)+a`ixVec`1)+a`ixVec`0) /
               (x*(x*(x*(x*(x*(x*(x*(a`ixVec`15) + a`ixVec`14)+a`ixVec`13)+a`ixVec`12)+a`ixVec`11)+a`ixVec`10)+a`ixVec`9)+a`ixVec`8)


-- strict variant for foldl is foldl'
-- tmp_rat_foldl :: RealFloat r => r -> [r] -> [r] -> r
tmp_rat_foldl :: SpecReal -> Array D DIM1 SpecReal -> Array D DIM1 SpecReal -> SpecReal
tmp_rat_foldl x as bs = let reduce xs = (R.foldAllS (\b c -> x*b + c) 0 xs)
                        in reduce as / reduce bs

smallcasel :: SpecReal -> SpecReal
smallcasel q = q * tmp_rat_foldl (0.180625 - q*q) r_a_small r_b_small

tmp_intermediatel :: SpecReal -> SpecReal
tmp_intermediatel q = tmp_rat_foldl (q - 1.6) r_a_interm r_b_interm

tmp_taill ::  SpecReal -> SpecReal
tmp_taill q = tmp_rat_foldl (q - 5.0) r_a_tail r_b_tail


gaussianElem :: F -> F
gaussianElem q =
        let dq = q - 0.5
        in if( abs dq <= 0.425 ) then
               smallcasel dq -- tmp_small_case dq
           else
               let pp = if dq < 0.0 then q else (1.0 - q)
                   s  = sqrt (0.0 - (log pp))
                   x  = if (s <= 5.0) then tmp_intermediatel s else tmp_taill s
               in if dq < 0.0 then (-x) else x

gaussian :: Array D DIM1 F -> Array D DIM1 F
gaussian = R.map gaussianElem


------------------------------------------------------------
--------------------- Brownian Bridge ----------------------
------------------------------------------------------------

brownian_bridge_gen :: Pricing_Data -> Array D DIM1 SpecReal -> [[SpecReal]]
brownian_bridge_gen conf@Pricing_Data{..} = undefined
  --  arrangePerDate . map (mkBBridge conf) . divideInto md_dim

arrangePerDate :: [Array D DIM1 SpecReal] -> [[SpecReal]]
arrangePerDate vs = let (Z :. l) = R.extent (head vs)
                    in [ map (R.! (Z :. k)) vs | k <- [0..l-1] ]

divideInto :: Int -> Array D DIM1 SpecReal -> [Array D DIM1 SpecReal]
divideInto n arr = map takeEachN [0..n-1]
   where
    takeEachN i = 
      let (Z :. arrN) = R.extent arr
          m = arrN `div` n + if arrN `mod` n >= i then 1 else 0
      in R.fromFunction (Z :. m) (\(Z :. j) -> arr `R.index` (Z :. i + j*n))
    

mkBBridge :: Pricing_Data -> [SpecReal] -> VB.Vector SpecReal
mkBBridge Pricing_Data{..} xs = runST mkBBridgeST
    where mkBBridgeST :: ST s (VB.Vector SpecReal)
          mkBBridgeST = do v <- VM.new md_nb_path_dates
                           res <- fillRec 0 v
                           VB.unsafeFreeze res
          fillRec n v | n == md_nb_path_dates = return v
                      | n == 0 = do VM.write v (bb_bi VB.! 0-1) (bb_sd VB.! 0 * head xs)
                                    fillRec (n+1) v
                      | otherwise = do
                          let lb  = bb_li VB.! n - 1
                              ub  = bb_ri VB.! n - 1
                              mid = bb_bi VB.! n - 1
                              zi  = xs !! n
                          wk <- VM.read  v ub
                          let tmp = bb_rw VB.! n * wk + bb_sd VB.! n * zi
                          if lb == -1 then VM.write v mid tmp
                                      else do z <- VM.read v lb
                                              VM.write v mid (tmp+z*bb_lw VB.! n)
                          fillRec (n+1) v


------------------------------------------------------------
---------------------  BLACK SCHOLES  ----------------------
------------------------------------------------------------

zipWithLen :: Int -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLen 0 op x  y  = []
zipWithLen p op [] y  = []
zipWithLen p op x  [] = []
zipWithLen p op (x:xs) (y:ys) = (x `op` y) : zipWithLen (p-1) op xs ys


-- for (i = dim * npathdates - 1; i >= dim; i--) {
--		priv_arr->md_zd[i] -= priv_arr->md_zd[i - dim];
--	}
mkDeltas :: Num r => [[r]] -> [[r]]
mkDeltas rows@(row1:restrows) = row1 : zipWith (zipWith (-)) restrows rows

-- INCORRECT!!! it does \sum_{k=0}^{md_dim} instead of \sum_{k=0}^{j}!!!
-- correlate_deltas :: Pricing_Data -> [[SpecReal]] -> [[SpecReal]]
-- correlate_deltas Pricing_Data{..} zds
--     =  map (\zr -> map (sum . zipWith (*) zr) md_c) zds

-- Correlate delta matrix using C matrix:
--  this is done for inst_num many computations, with different model data;
--  but we assume inst_num == 1 for now.
-- $$ czd_{i,j} = \sum_{k=0}^{j} zd_{i,k} \cdot c_{j,k} $$
-- TODO: make md_c an array of lists, so that (md_c !! j) is constant time (?)
--       perhaps combine sum . zipWithLen in a tail recursive function
--       modify the configuration to make md_c triangular (?)
--       write nicer?
correlate_deltas :: Pricing_Data -> [[SpecReal]] -> [[SpecReal]]
correlate_deltas Pricing_Data{..} zds
    = let f zi = map ( \j -> (sum . zipWithLen (j+1) (*) zi) (md_c !! j) ) [0..md_dim-1]
      in  map f zds
--    =  let md_cj = map (\j -> take (j+1) (md_c !! j)) [0..md_dim-1]
--       in map (\zi -> map (sum . zipWith (*) zi) md_cj) zds


-- step 2): multiply start prices with correlated deltas (scanl1)
--     trajWF[i][j] = S[j] * prod_{l=0}^{i} exp(czd[l][j]*vol[l][j] + dr[l][j])
--       with S[j] = trajWF[-1][j] in C code.
mkPrices :: Pricing_Data -> [[SpecReal]] -> [[SpecReal]]
mkPrices Pricing_Data{..} noises
    = let combineVs n_row vol_row dr_row
              = zipWith (+) (zipWith (*) n_row vol_row) dr_row
          e_rows = map (map exp) (zipWith3 combineVs noises md_vols md_drifts)
                                   -- czd[l][j]*vol[l][j] + dr[l][j]
      in tail (scanl (zipWith (*)) md_starts e_rows)

black_scholes :: Pricing_Data -> [[SpecReal]] -> [[SpecReal]]
black_scholes l = mkPrices l . correlate_deltas l . mkDeltas


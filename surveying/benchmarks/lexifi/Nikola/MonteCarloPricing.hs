-----------------------------------------------
--- BEGIN IMPORTS                           ---
-----------------------------------------------

import System.Environment -- access to arguments etc.

import qualified Data.Bits as DB -- bit twiddling with shift and xor
import qualified Data.List as DL     -- HOFs for lists

import qualified Data.Vector as VB   -- vector, boxed
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU

import Control.Monad.ST

-- all types are imported from here
import PricingTypesS      -- data types used

-- the actual computation to be carried out is defined in here
import PricingExample1 -- function example<N>_init::Integer->Pricing_Data

--import SobolUtil(floor_log2)

type Index = Integer


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

-- independent formula
-- LENGTH(RET) = LENGTH(sobol_dirVs)
sobolInd_  :: Pricing_Data -> Index -> [ Elem ]
sobolInd_ Pricing_Data{..} n =
        let indices  = filter (DB.testBit (grayCode n)) [ 0..sobol_bit_count-1 ]
            xorVs vs = DL.foldl' DB.xor 0 [ vs VB.! i | i <- indices ]
        in map xorVs sobol_dirVs

-- sobolRec_ :: Pricing_Data -> Index -> [Elem] -> [Elem]
-- sobolRec_ Pricing_Data{..} n prev = zipWith xor prev dirVs
--    where dirVs = [ vs DA.! bit | vs <- sobol_dirVs]
--          bit   = lsb0 n -- rmt n

-- or compute by recurrence
-- INVAR: n >= 0 \implies sobolInd_ l (n+1) \equiv sobolRec_ l (n+1) (sobolInd_ l n)
sobolRec_ :: Pricing_Data -> [Elem] -> Index -> [Elem]
sobolRec_ Pricing_Data{..} prev n = zipWith DB.xor prev dirVs
  where dirVs = [ vs VB.! bit | vs <- sobol_dirVs]
        bit   = lsb0 n -- rmt n


sobolInd :: Pricing_Data -> Index -> [ SpecReal ]
sobolInd l n = map norm (sobolInd_ l n)
    where norm = ( / sobol_divisor l ) . fromIntegral


------------------------------------------------------------
------------------------------------------------------------
----- given a random number, generate a gausian number -----
------------------------------------------------------------
------------------------------------------------------------

----------------------------
--- BEGIN Constant Lists ---
----------------------------

-- 1. For smallcase:
a_small ::[SpecReal]
a_small = [       3.387132872796366608,
                  133.14166789178437745,
                  1971.5909503065514427,
                  13731.693765509461125,
                  45921.953931549871457,
                  67265.770927008700853,
                  33430.575583588128105,
                  2509.0809287301226727]
r_a_small = reverse a_small

b_small ::[SpecReal]
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
a_interm :: [SpecReal]
a_interm = [      1.42343711074968357734,
                  4.6303378461565452959,
                  5.7694972214606914055,
                  3.64784832476320460504,
                  1.27045825245236838258,
                  0.24178072517745061177,
                  0.0227238449892691845833,
                  7.7454501427834140764e-4]
r_a_interm = reverse a_interm

b_interm ::[SpecReal]
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
a_tail ::[SpecReal]
a_tail   = [      6.6579046435011037772,
                  5.4637849111641143699,
                  1.7848265399172913358,
                  0.29656057182850489123,
                  0.026532189526576123093,
                  0.0012426609473880784386,
                  2.71155556874348757815e-5,
                  2.01033439929228813265e-7]
r_a_tail = reverse a_tail

b_tail ::[SpecReal]
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

tmp_rat_evalL :: SpecReal -> [SpecReal] -> SpecReal
tmp_rat_evalL x a =
               (x*(x*(x*(x*(x*(x*(x*(a!!7) + a!!6)+a!!5)+a!!4)+a!!3)+a!!2)+a!!1)+a!!0) /
               (x*(x*(x*(x*(x*(x*(x*(a!!15) + a!!14)+a!!13)+a!!12)+a!!11)+a!!10)+a!!9)+a!!8)


-- strict variant for foldl is foldl'
-- tmp_rat_foldl :: RealFloat r => r -> [r] -> [r] -> r
tmp_rat_foldl :: SpecReal -> [SpecReal] -> [SpecReal] -> SpecReal
tmp_rat_foldl x as bs = let reduce xs = (DL.foldl' (\b c -> x*b + c) 0 xs)
                        in reduce as / reduce bs

smallcasel :: SpecReal -> SpecReal
smallcasel q = q * tmp_rat_foldl (0.180625 - q*q) r_a_small r_b_small

tmp_intermediatel :: SpecReal -> SpecReal
tmp_intermediatel q = tmp_rat_foldl (q - 1.6) r_a_interm r_b_interm

tmp_taill ::  SpecReal -> SpecReal
tmp_taill q = tmp_rat_foldl (q - 5.0) r_a_tail r_b_tail

gaussianElem ::  SpecReal -> SpecReal
gaussianElem q =
        let dq = q - 0.5
        in if( abs dq <= 0.425 ) then
               smallcasel dq -- tmp_small_case dq
           else
               let pp = if dq < 0.0 then q else (1.0 - q)
                   s  = sqrt (0.0 - (log pp))
                   x  = if (s <= 5.0) then tmp_intermediatel s else tmp_taill s
               in if dq < 0.0 then (-x) else x

gaussian :: [SpecReal] -> [SpecReal]
gaussian lst = map gaussianElem lst
-- can be replaced by another method do make a std.normal gaussian
-- distrib. from a uniform [0,1] distrib


------------------------------------------------------------
--------------------- Brownian Bridge ----------------------
------------------------------------------------------------

brownian_bridge_gen :: Pricing_Data -> [SpecReal] -> [[SpecReal]]
brownian_bridge_gen conf@Pricing_Data{..} =
    arrangePerDate . map (mkBBridge conf) . divideInto md_dim

arrangePerDate :: [VB.Vector SpecReal] -> [[SpecReal]]
arrangePerDate vs = let l = VB.length (head vs)
                    in [ map (VB.! k) vs | k <- [0..l-1] ]

divideInto :: Int -> [SpecReal] -> [[ SpecReal ]]
divideInto n xs = let takeEachN [] = []
                      takeEachN xs = head xs : takeEachN (drop n xs)
                  in map takeEachN  (take n (DL.tails xs))

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


------------------------------------------------------------
---- OPTIMIZATIONS: TILING ETC.                          ---
------------------------------------------------------------

-- TODO: this works only when `chunk' divides `num_iters' => make
--       it correct in the general case
tiledSkeleton :: Pricing_Data -> Integer -> ((Integer,Integer) -> SpecReal) -> SpecReal
tiledSkeleton conf chunk fun =
  let divides = (num_iters conf) `mod` chunk == 0
      extra   = if (divides) then [] else [num_iters conf]
      iv = zip [1,chunk+1..] ([chunk, 2*chunk .. num_iters conf] ++ extra )
--[(i+1, i+chunk) | j <- [1..(num_iters conf) `div` chunk], let i=(j-1)*chunk]
  in (sum . map fun) iv

sobolRecMap conf (l, u) =
  let a = sobolInd_ conf l
      norm = ( / sobol_divisor conf ) . fromIntegral
  in  map (map norm) (scanl (sobolRec_ conf) a [l..u-1])
-- sobolRecMap conf 1 n


------------------------------------------------------------
-------------  FINALLY MAP-REDUCE THE RESULT  --------------
------------------------------------------------------------


-- The Monte-Carlo aggregation needs to average over payoffs from
-- all samples (one sample being a set of trajectories).
mc_red :: Pricing_Data -> [[[SpecReal]]] -> SpecReal
mc_red config samples = factor * sum gains
    where gains = map (payoff config) samples
          payoff = product_payoff config
          factor = 1 / (fromIntegral (num_iters config))
          -- length samples == num_iters config


mc_pricing :: Pricing_Data -> SpecReal -- output: one final price
mc_pricing l = let  zs :: [[[SpecReal]]]
                    zs = map ( black_scholes l
                               . brownian_bridge_gen l
                               . gaussian
                               . sobolInd l) [1..num_iters l]
                    -- payoff = call_payoff 4000
               in  (mc_red l zs)

mc_pricing_chunk :: Pricing_Data -> (Integer, Integer) -> SpecReal
mc_pricing_chunk conf (lb,ub) = let payoff = product_payoff conf
                                    factor = 1 / (fromIntegral (num_iters conf))
                                    zs :: [SpecReal]
                                    zs = map ( payoff conf
                                               . black_scholes conf
                                               . brownian_bridge_gen conf
                                               . gaussian )
                                             ( sobolRecMap conf (lb,ub) )
                                in factor * sum zs



-----------------------------------------------
--- BEGIN MAIN                              ---
-----------------------------------------------
--- ghc -O2 -XFlexibleInstances -XRecordWildCards -o test MonteCarloPricing.hs


main :: IO()
main = do args <- getArgs
          let n = if null args then 100000 else read (head args)
              conf = example_init n -- all examples should export this name
              ----------------------
              res    = mc_pricing conf
              resopt = tiledSkeleton conf 32 (mc_pricing_chunk conf)
          putStrLn ("Config: " ++ show n ++ " iterations")
          putStrLn ("Computed opt: " ++ show resopt)
          putStrLn ("Computed:     " ++ show res)


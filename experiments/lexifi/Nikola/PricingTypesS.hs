{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module PricingTypesS where

import qualified Data.Word as DW
import qualified Data.Array as DA
import qualified Data.Vector as V -- undecided if boxed or unboxed

type Elem     = DW.Word32
type SpecReal = Double

data Pricing_Data = Pricing_Data {
         -- sobol:
         num_iters          :: Integer,
         sobol_bit_count    :: Int, -- bit precision, here <32 (Word32)
         sobol_dim          :: Int, -- problem dim.2 (md_dim*md_nb_path_dates)
         sobol_divisor      :: SpecReal, -- for norming to [0..1] interval
         sobol_dirVs        :: [V.Vector Elem],-- dir. vecs, dim.1 x dim.2
                            -- efficient dim1 lookup essential for sobolFF,
                            -- dim2 is again sobol_dim (observables * dates)
         -- Model Data: Observables, dates, relationship
         md_dim             :: Int, -- no. of observables
         md_nb_path_dates   :: Int, -- no. of dates to consider
         md_c               :: [[SpecReal]], -- correlations (obs x obs)
         md_vols            :: [[SpecReal]], -- volatilities (per obs/date)
         md_drifts          :: [[SpecReal]], -- drifts (per obs/date)
         md_starts          :: [SpecReal],   -- start values for observables
         -- other model attributes
         model_deter_vals   :: [SpecReal],   -- deterministic values(???)
         model_discounts    :: [SpecReal],   -- discounts for all path dates

         -- brownian bridge parameters
         bb_l               :: Int,
         bb_sd              :: V.Vector SpecReal,
         bb_lw              :: V.Vector SpecReal,
         bb_rw              :: V.Vector SpecReal,
         bb_bi              :: V.Vector Int,
         bb_li              :: V.Vector Int,
         bb_ri              :: V.Vector Int,
         ----------------------------
         -- payoff for the actual example
         product_payoff     :: Payoff
        }
       deriving (Show,Eq)

no_Data :: Pricing_Data
no_Data =  Pricing_Data undefined undefined undefined undefined undefined
           undefined undefined undefined undefined undefined
           undefined undefined undefined undefined undefined
           undefined undefined undefined undefined undefined
           undefined

type Payoff = Pricing_Data -> [[SpecReal]] -> SpecReal
instance Show Payoff where
    show _ = "<payoff function not shown>"
instance Eq Payoff where
    (==) _ _ = error "payoff function not comparable"

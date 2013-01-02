{-# OPTIONS -XTypeSynonymInstances -XFlexibleInstances #-}
module PricingTypesS where

import qualified Data.Word as DW
import qualified Data.Array as DA
import qualified Data.Vector as V -- undecided if boxed or unboxed
import Data.Vector(Vector)

import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate (Array, DIM1, DIM2, DIM3, Acc)

type Elem     = DW.Word32
type SpecReal = Double
type Index    = Int

data Pricing_Data = Pricing_Data {
         -- sobol:
         num_iters          :: Index, -- no. of samples (index into sobol seq)
         sobol_bit_count    :: Int, -- bit precision, here <32 (Word32)
         sobol_dim          :: Int, -- problem dim.2 (md_dim*md_nb_path_dates)
         sobol_divisor      :: SpecReal, -- for norming to [0..1] interval
         sobol_dirVs        :: Vector (Vector Elem),-- dir. vecs, dim.1 x dim.2
                            -- dim1 is bit count
                            -- dim2 is sobol_dim (observables * dates)
         -- Model Data: Observables, dates, relationship
         md_dim             :: Int, -- no. of observables
         md_nb_path_dates   :: Int, -- no. of dates to consider
         md_c               :: Vector (Vector SpecReal), -- correlations (obs x obs)
         md_vols            :: Vector (Vector SpecReal), -- volatilities (per obs/date)
         md_drifts          :: Vector (Vector SpecReal), -- drifts (per obs/date)
         md_starts          :: Vector SpecReal,   -- start values for observables
         -- other model attributes
         model_deter_vals   :: Vector SpecReal,   -- deterministic values(???)
         model_discounts    :: Vector SpecReal,   -- discounts for all path dates

         -- brownian bridge parameters
         bb_l               :: Int,
         bb_sd              :: Vector SpecReal,
         bb_lw              :: Vector SpecReal,
         bb_rw              :: Vector SpecReal,
         bb_bi              :: Vector Int,
         bb_li              :: Vector Int,
         bb_ri              :: Vector Int,
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

type Payoff = Pricing_Data -> Acc (Array DIM3 SpecReal) -> Acc (Array DIM1 SpecReal)

instance Show Payoff where
    show _ = "<payoff function not shown>"
instance Eq Payoff where
    (==) _ _ = error "payoff function not comparable"

-- helper
mkVector2 :: [[a]] -> Vector (Vector a)
mkVector2 = V.fromList . (map V.fromList)

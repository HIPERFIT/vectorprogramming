{-# LANGUAGE RecordWildCards #-}
module PricingExample1 (example_init) where

import PricingTypesS
import qualified Data.Vector as V -- undecided if boxed or unboxed

{-
Expected output:

Config: 100000 iterations
Computed opt: 169.8643135062839
Computed:     169.86431350628382
-}

-- simple European option on one underlying
example_init :: Integer -> Pricing_Data
example_init num_it = no_Data
                        { num_iters = num_it
                        , sobol_bit_count = 30
                        , sobol_dim = 1
                        , sobol_divisor = fromIntegral (2^30)
                        , sobol_dirVs
                            = [ V.fromList [2^k | k <- [29,28..0]] ]
                        , md_dim = 1
                        , md_nb_path_dates = 1
                        , md_c      = [[ 1.00000000000000000000000000000000]]
                        , md_vols   = [[ 0.19000000000000002997602166487923]]
                        , md_drifts = [[-0.02835124107323035158589874527024]]
                        , md_starts = [3758.05000000000018189894035458564758]
                        , model_deter_vals = [0.99976705777418484188956426805817]
                        , model_discounts  = [0.98007005548842429654143870720873]
                        , bb_l = 1
                        , bb_sd = V.fromList [1.00136892603445470939504957641475]
                        , bb_bi = V.fromList [1]
                        , product_payoff = call_payoff 4000
                        }

-- For a simple European call, there is only one value to consider.

-- call_payoff :: SpecReal -> Pricing_Data -> [[SpecReal]] -> SpecReal
call_payoff :: SpecReal -> Payoff
call_payoff strike Pricing_Data{..}
  = \((price:_):_) -> max 0 (price - strike) * (model_deter_vals!!0) * (model_discounts!!0)
{-
  -- where
  --  price = take 1 (take 1 input)
-}

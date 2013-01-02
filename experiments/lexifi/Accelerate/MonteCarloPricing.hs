module MonteCarloPricing (mc_pricing) where

import Sobol (sobol)
import Gaussian (gaussian)
import BrownianBridge (brownianBridge)
import BlackScholes (blackScholes)
import PricingTypesS

import Prelude hiding (map, sum)
import Data.Array.Accelerate

-- sum :: (Shape sh, IsNum a) => Acc (Array sh a) -> Acc (Scalar a)
-- sum = foldAll (+) 0

-- The Monte-Carlo aggregation needs to average over payoffs from
-- all samples (one sample being a set of trajectories).
mc_red :: Pricing_Data -> Acc (Array DIM3 SpecReal) -> Acc (Scalar SpecReal)
mc_red config samples = map (constant factor *) $ sum gains
    where gains = payoff samples
          payoff :: Acc (Array DIM3 SpecReal) -> Acc (Vector SpecReal)
          payoff = product_payoff config $ config
          factor :: SpecReal
          factor = 1 / (Prelude.fromIntegral (num_iters config))
          -- length samples == num_iters config

mc_pricing :: Pricing_Data -> Acc (Scalar SpecReal)
mc_pricing l = mc_red l . blackScholes l . brownianBridge l . gaussian $ sobol l indices
  where
    indices :: Array DIM1 Index
    indices = fromList (Z :. num_iters l) [0 .. num_iters l]
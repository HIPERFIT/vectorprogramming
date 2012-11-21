{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MonteCarloPricing where

import Data.Array.Nikola.Backend.CUDA as C

import PricingTypesS

type DIM3 = DIM2 :. Exp Ix

type Index = Integer
type F = Double


--mapArray :: (Array shA F -> Array shB F) -> Array shC F -> Array (shC-shA+shB)

--fromF :: shA -> (shA -> Array shB F) -> Array (shA :++: shB) F

mc_pricing :: Pricing_Data -> Exp F -- output: one final price
mc_pricing l = 
  let 
    ix = Z :. (fromInteger $ num_iters l)
    zs :: Array D DIM2 (Exp F)
    zs = black_scholes l
       . brownian_bridge_gen l
       . gaussian
       . sobolInd l $ ix
         -- payoff = call_payoff 4000
  in  (mc_red l zs)

mc_red :: Pricing_Data -> Array D DIM2 (Exp F) -> Exp F
mc_red = undefined

black_scholes :: Pricing_Data -> Array D DIM2 (Exp F) -> Array D DIM2 (Exp F)
black_scholes l = undefined


brownian_bridge_gen :: Pricing_Data -> Array D DIM1 (Exp F) -> Array D DIM2 (Exp F)
brownian_bridge_gen conf@Pricing_Data{..} = undefined

gaussian :: Array D DIM1 (Exp F) -> Array D DIM1 (Exp F)
gaussian lst = undefined

sobolInd :: Pricing_Data -> DIM1 -> Array D DIM1 (Exp F)
sobolInd l n = map norm (sobolInd_ l n)
    where norm = ( / sobol_divisor l ) . fromIntegral



-- independent formula
-- LENGTH(RET) = LENGTH(sobol_dirVs)
sobolInd_  :: Pricing_Data -> Index -> [ Elem ]
sobolInd_ Pricing_Data{..} n =
        let indices  = filter (DB.testBit (grayCode n)) [ 0..sobol_bit_count-1 ]
            xorVs vs = foldl DB.xor 0 [ vs VB.! i | i <- indices ]
        in map xorVs sobol_dirVs





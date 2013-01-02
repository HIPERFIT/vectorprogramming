module BlackScholes (blackScholes) where

import Data.Array.Accelerate (Acc, Array, DIM1, DIM2, DIM3)
import qualified Data.Array.Accelerate as Acc
import PricingTypesS

blackScholes :: Pricing_Data -> Acc (Array DIM3 SpecReal) -> Acc (Array DIM3 SpecReal)
blackScholes = undefined

zipWithLen p op x y = Acc.take p $ Acc.zipWith op x y

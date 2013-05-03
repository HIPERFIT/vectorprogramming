{-# LANGUAGE RecordWildCards #-}
module BlackScholes where

import Options

-- Black-Scholes formula
blackScholes :: EurOpt -> Float
blackScholes (EurOpt{..}) = case opttype of
     Call -> s0*cnd(d1)-strike*exp(-riskless * fexpiry)*cnd(d2)
     Put  -> strike*exp(-riskless * fexpiry)*cnd(-d2)-s0*cnd(-d1)
  where
    d1 = (log(s0/strike)+(riskless+volatility*volatility/2.0) * fexpiry)/(volatility*sqrt(fexpiry))
    d2 = d1-volatility*sqrt(fexpiry)
    fexpiry :: Float
    fexpiry = fromIntegral expiry
    
    -- Cumulative normal distribution
    cnd :: Float -> Float
    cnd x = if x < 0 then 1.0 - w else w
     where
      (a1,a2,a3,a4,a5) = (0.31938153, -0.356563782, 1.781477937, -1.821255978, 1.330274429)
      l = abs x
      k = 1.0 / (1.0 + 0.2316419 * l)
      w = 1.0 - 1.0 / sqrt(2*pi)*exp(-l*l/2.0) * (a1*k + a2*k*k + a3*(k^(3 :: Int))
          + a4*(k^(4 :: Int)) + a5*(k^(5 :: Int)))


blackScholesPortfolio :: [EurOpt] -> [Float]
blackScholesPortfolio = map blackScholes
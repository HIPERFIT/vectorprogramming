module Options where

data OptType = Call | Put
     deriving Eq

-- ^ European option
data EurOpt = EurOpt
       { opttype    :: OptType -- ^ Call or Put option?
       , s0         :: Float   -- ^ Current price of underlying
       , strike     :: Float   -- ^ Strike price
       , expiry     :: Int     -- ^ Expiry in years
       , riskless   :: Float   -- ^ Riskless interest rate
       , volatility :: Float
       }

-- ^ A sample option for testing purposes
-- Price: 5.349524 (with 2048 time steps)
sampleOpt :: EurOpt
sampleOpt = EurOpt
       { opttype    = Call
       , s0         = 60.0
       , strike     = 65.0
       , expiry     = 1
       , riskless   = 0.1
       , volatility = 0.2
       }


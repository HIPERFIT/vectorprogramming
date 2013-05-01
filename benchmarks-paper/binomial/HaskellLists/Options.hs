module Options where

-- ^ Riskless interest rate
riskless :: Float
riskless = 0.1

-- ^ Volatility
volatility :: Float
volatility = 0.2

data OptType = Call | Put
     deriving Eq

-- ^ European option
data EurOpt = EurOpt
       { opttype    :: OptType -- ^ Call or Put option?
       , s0         :: Float   -- ^ Current price of underlying
       , strike     :: Float   -- ^ Strike price
       , expiry     :: Int     -- ^ Expiry in years
       , numSteps   :: Int
       }

-- ^ A sample option for testing purposes
-- Price: 5.348364
sampleOpt :: EurOpt
sampleOpt = EurOpt
       { opttype    = Call
       , s0         = 60.0
       , strike     = 65.0
       , expiry     = 1
       , numSteps   = 1000
       }

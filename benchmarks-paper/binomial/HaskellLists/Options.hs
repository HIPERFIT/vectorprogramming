module Options where

-- Options represented as 5-tuples. It would be nicer if we could use
-- records, but neither Accelerate nor Nikola supports records on the
-- GPU.

-- We standardize on European Call options in the benchmarks, and we
-- thus leave out parameters for European/American and call/put.

-- ^ Should always be interpreted as a European Call option
type Option =
         ( Float -- ^ Current price of underlying
         , Float -- ^ Strike price               
         , Int   -- ^ Expiry in years            
         , Float -- ^ Riskless interest rate     
         , Float -- ^ Volatility
         )

-- ^ A sample option for testing purposes
-- Price: 5.349524 (with 2048 time steps)
sampleOpt :: Option
sampleOpt = (60.0, 65.0, 1, 0.1, 0.2)

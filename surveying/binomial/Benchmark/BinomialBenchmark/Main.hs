module BinomialBenchmark.Main(runTest)
where

import qualified Criterion.Main as C

-- | run the tests of a binomial american option pricer, given by the 'binom'
-- function.
runTest binom = do
  let benchmarks = [ C.bench (show years) $ C.nf binom years
                   | years <- [1, 8]] -- , 10, 30]]
  C.defaultMain benchmarks



module BinomialBenchmark.Main(runTest, runTestIO)
where

import qualified Criterion.Main as C

-- | run the tests of a binomial american option pricer, given by the 'binom'
-- function.
runTest binom = do
  C.defaultMain $ benchmarkYears (C.nf binom)

runTestIO binom = do
  C.defaultMain $ benchmarkYears binom

benchmarkYears binomBench = [ C.bench (show years ++ " years simulated") $
                  binomBench years
                 | years <- [1, 2]]

module BinomialBenchmark.Main(runTest, runTestIO, runTestWith, cfgModSummaryFile)
where

import Data.Monoid

import qualified Criterion.Config as CCfg
import qualified Criterion.Main as C
import System.Environment(getArgs, withArgs)
import System.FilePath(takeBaseName,replaceBaseName)

-- | run the tests of a binomial american option pricer, given by the 'binom'
-- function.
runTest bench = do
  C.defaultMain $ benchmarkYears (C.nf bench)

runTestIO bench = do
  C.defaultMain $ benchmarkYears bench

runTestWith cfgMod bench = do
  args <- getArgs
  (cfg,_) <- C.parseArgs CCfg.defaultConfig C.defaultOptions args
  -- we want to override given cmdline args, so we hide them from 'defaultMainWith'
  withArgs [] $
    C.defaultMainWith (cfgMod cfg) (return ())  $ benchmarkYears (C.nf bench)

-- | Function to modify the basename of the summaryfile.
cfgModSummaryFile :: (String -> String) -> CCfg.Config -> CCfg.Config
cfgModSummaryFile sf cfg = 
  cfg {CCfg.cfgSummaryFile = Last $ do
    last <- getLast $ CCfg.cfgSummaryFile cfg
    return $ replaceBaseName last $ sf (takeBaseName last)
    }

-- | Default benchmark for the binomial pricer.
benchmarkYears binomBench = [ C.bench (show years) $
                  binomBench years
                  | years <- [1]] --,2,4,8,16,32,64]]

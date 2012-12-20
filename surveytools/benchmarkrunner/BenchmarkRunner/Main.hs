module Main -- (runTest, runTestIO, runTestWith, cfgModSummaryFile)
where

import Data.Monoid

import qualified Criterion.Config as CCfg
import qualified Criterion.Main as C
import System.Environment(getArgs, withArgs)
import System.FilePath(takeBaseName,replaceBaseName)

import System.IO(hSetBuffering, stdout,BufferMode(..),stdin, hGetLine)

import BenchmarkRunner.External

-- | run the tests of a binomial american option pricer, given by the 'binom'
-- function.
--runTest :: Num a => Show a => NFData b => (a -> b) -> IO ()
{-
runTest bench = do
  -- Always do line buffering on stdout!
  hSetBuffering stdout LineBuffering
  args <- (map read . words) `fmap` hGetLine stdin
  C.defaultMain $ benchmarkInts (args :: [Int]) (C.nf bench)
-}

runTestIO :: C.Benchmarkable b => (String -> b) -> IO ()
runTestIO bench = do
  -- Always do line buffering on stdout!
  hSetBuffering stdout LineBuffering
  args <- words `fmap` hGetLine stdin
  C.defaultMain $ benchmarks args bench

{-
runTestWith cfgMod bench = do
  -- Always do line buffering on stdout!
  hSetBuffering stdout LineBuffering
  args <- getArgs
  (cfg,_) <- C.parseArgs CCfg.defaultConfig C.defaultOptions args
  args_ <- (map read . words) `fmap` hGetLine stdin
  -- we want to override given cmdline args, so we hide them from 'defaultMainWith'
  withArgs [] $
    C.defaultMainWith (cfgMod cfg) (return ()) $ benchmarkInts (args_ :: [Int]) (C.nf bench)

-- | Function to modify the basename of the summaryfile.
cfgModSummaryFile :: (String -> String) -> CCfg.Config -> CCfg.Config
cfgModSummaryFile sf cfg =
  cfg {CCfg.cfgSummaryFile = Last $ do
    last <- getLast $ CCfg.cfgSummaryFile cfg
    return $ replaceBaseName last $ sf (takeBaseName last)
    }

-}
benchmarks :: C.Benchmarkable b => [String] -> (String -> b) -> [C.Benchmark]
benchmarks args bench =
  map (\i -> C.bench (show i) $ bench i) args

main = do
  exec:rest <- getArgs
  extProc <- initialiseExt exec [] -- "/bin/bash" ["-c", exec]
  withArgs rest $ runTestIO $ benchmarkExt extProc
  terminateExt extProc

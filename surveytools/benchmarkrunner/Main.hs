module Main where

-- Small script to run the various test scripts

import Control.Applicative
import Control.Monad(when, filterM, forM, (<=<))

import Data.List (nub)
import Data.Maybe (isJust)

import System.FilePath((</>), normalise)
import System.Environment(getArgs, getProgName)
import System.Exit
import System.Process(rawSystem, readProcess)
import System.Directory

-- The various benchmarks, and the version of ghc (which hsenv) they use.
platforms = [
     ("CUDA", "vanilla-GHC7.4.2")
    , ("Nikola", "nikola-GHC7.4.2")
    , ("R", "vanilla-GHC7.4.2")
    , ("Repa", "accelerate-github-GHC7.6.1")
    , ("Vector", "vanilla-GHC7.4.2")
    , ("CPP", "vanilla-GHC7.4.2")
--   ("Accelerate", "accelerate-hackage-GHC7.4.1")
-- , ("DPH", ghc742)
   -- , ("Feldspar", ghc742)
   -- , ("Obsidian", ghc742)
   -- , ("OpenCL", "vanilla-GHC7.4.2")
    ]

hsEnvs = nub $ map snd platforms
platformName = map fst

main = do
  args <- getArgs
  case args of
    x | x == ["-h"] || x == [] -> getProgName >>= \n -> putStrLn $ "Usage: " ++ n ++ " [-r] [platforms]\n"
                                    ++ "  where valid platforms are :" ++ show (platformName platforms) ++ "\n"
                                    ++ "  or \"all\" to run all benchmarks on all platforms!\n"
                                    ++ "  '-r' rebuilds the test routines used in the benchmarks."

    ["-r"] -> do
      tag <- logtag
      putStrLn $ "(" ++ tag ++ ") Rebuilding test routine"
      whileTrue (rebuildTestRoutine tag) hsEnvs

    ["all"] -> runBenchmarks platforms
    ns      -> runBenchmarks (filter ((`elem` ns) . fst) platforms)

-- Run rebuild script
rebuildTestRoutine tag env = do
  exitcode <- rawSystem "./rebuildTestRoutine.sh" [env,tag]
  isSuccess ("rebuildTestRoutine of " ++ env ++ " failed!") exitcode

-- Run the benchmarks on the given series of platforms
runBenchmarks ns = do
  benchmarks <- getBenchmarks ns
  tag <- logtag
  putStrLn $ "(" ++ tag ++ ") Executing the following experiments:"
  -- mkSummaryDir tag
  print benchmarks
  whileTrue (criterion tag) benchmarks

-- Go through the directory hierarchy and collect the existing
-- benchmarks, and remove all that doesn't have a matching platform in
-- the platforms list
getBenchmarks :: [(String, String)] -> IO [(String, String, String)]
getBenchmarks ns = do
  -- Which benchmarks should we run
  pwd <- getCurrentDirectory
  let dir = pwd </> "../../benchmarks"
  benchmark_dirs <- retrieveSubdirectories dir
  -- And then, on which platforms?
  concatForM benchmark_dirs $ \benchmark -> do
    ps <- retrieveSubdirectories $ dir </> benchmark
    return [(benchmark, p, env) | p <- ps, (p', env) <- ns, p == p']
 where
   retrieveSubdirectories :: FilePath -> IO [FilePath]
   retrieveSubdirectories dir = do
     xs <- removeDotdirs `fmap` getDirectoryContents dir
     filterM (doesDirectoryExist . (dir </>)) xs
   removeDotdirs = filter ((/= '.') . head)
   concatForM ls f = concat `fmap` forM ls f

--
criterion tag (benchmark, platform, env) = do
  exitcode <- rawSystem "./run.sh" [benchmark,platform,env,tag]
  isSuccess ("running benchmark " ++ benchmark ++ ", " ++ platform ++ " (on " ++ env ++") failed!") exitcode

-- print a message on error, return True if successful.
isSuccess _ ExitSuccess = return True
isSuccess msg _ = putStrLn msg >> return False


--mkSummaryDir tag = rawSystem "mkdir" ["-p", "../summaries" </> tag]

-- the log-tag to assign to runs
logtag = head . lines <$> readProcess "date" ["+%Y-%m-%d,%H:%M:%S"] ""

-- small helper sequencer to exit early in case of error
whileTrue _ [] = return ()
whileTrue a (x:xs) = do
  res <- a x
  when res (whileTrue a xs)

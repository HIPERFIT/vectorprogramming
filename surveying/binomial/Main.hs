module Main where

-- Small script to run the various test scripts

import Control.Applicative
import Control.Monad(when)

import Data.List (nub)

import System.FilePath((</>))
import System.Environment(getArgs, getProgName)
import System.Exit
import System.Process(rawSystem, readProcess)

-- The various benchmarks, and the version of ghc (which hsenv) they use.
benchmarks = [
   --   ("Accelerate", "accelerate-hackage-GHC7.4.1")
     ("Cuda", "vanilla-GHC7.4.2")
   -- , ("DPH", ghc742)
   -- , ("Feldspar", ghc742)
      , ("Nikola", "nikola-GHC7.4.2")
   -- , ("Obsidian", ghc742)
   -- , ("OpenCL", "vanilla-GHC7.4.2")
    , ("R", "vanilla-GHC7.4.2")
    , ("Repa", "accelerate-hackage-GHC7.4.1")
    , ("Vector", "vanilla-GHC7.4.2")
    ]

hsEnvs = nub $ map snd benchmarks
benchNames = map fst

main = do
  args <- getArgs
  case args of
    x | x == ["-h"] || x == [] -> getProgName >>= \n -> putStrLn $ "Usage: " ++ n ++ " [-r] [Benchmarks]\n"
                                    ++ "  where valid benchmarks are :" ++ show (benchNames benchmarks) ++ "\n"
                                    ++ "  or \"all\" to run all the benchmarks!\n"
                                    ++ "  '-r' rebuilds the test routine used in the benchmarks."

    ["-r"] -> do
      tag <- logtag
      putStrLn $ "Rebuilding test routine (tag: " ++ tag ++ ")"
      whileTrue (rebuildTestRoutine tag) hsEnvs

    ["all"] -> do
      tag <- logtag
      putStrLn $ "Running tests (tag : " ++ tag ++ ")"
      mkSummaryDir tag
      whileTrue (criterion tag) benchmarks

    ns -> do
      tag <- logtag
      putStrLn $ "Running tests (tag : " ++ tag ++ ")"
      mkSummaryDir tag
      whileTrue (criterion tag) (filter ((`elem` ns) . fst) benchmarks)
  where
    criterion tag (b,env) = do
      exitcode <- rawSystem "./run.sh" [b,env,tag]
      isSuccess ("running benchmark " ++ b ++ " (on " ++ env ++") failed!") exitcode

    rebuildTestRoutine tag env = do
      exitcode <- rawSystem "./rebuildTestRoutine.sh" [env,tag]
      isSuccess ("rebuildTestRoutine of " ++ env ++ " failed!") exitcode

    -- print a message on error, return True if successful.
    isSuccess _ ExitSuccess = return True
    isSuccess msg _ = putStrLn msg >> return False

    mkSummaryDir tag = rawSystem "mkdir" ["summaries" </> tag]

    -- the log-tag to assign to runs
    logtag = head . lines <$> readProcess "date" ["+%Y-%m-%d,%H:%M:%S"] ""

    -- small helper sequencer to exit early in case of error
    whileTrue _ [] = return ()
    whileTrue a (x:xs) = do
      res <- a x
      when res (whileTrue a xs)

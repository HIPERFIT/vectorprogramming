module Main where

import Data.List
import Data.Maybe

import Distribution.Package
import Distribution.PackDeps
import Distribution.Version
import Distribution.Text

import System.Environment
import System.Exit

-- Return ExitSuccess if cabalfile given as cmdline arg 1 depends on
-- package arg 2 of version arg 3



main = do
  [cabalFile, depnameStr, depverStr] <- getArgs
  let depname = PackageName depnameStr
  let version = parseVersion depverStr
  Just descInfo <- loadPackage cabalFile
  let deps = filter (\(Dependency nm _) -> depname == nm) $ diDeps descInfo
  if or (map (\(Dependency _ range) -> withinRange version range) deps)
    then
      exitSuccess
    else
      exitFailure
  where
    parseVersion :: String -> Version
    parseVersion = fromJust . simpleParse

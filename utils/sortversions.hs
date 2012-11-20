module Main where

import Data.List
import Data.Maybe

import Distribution.Text
import Distribution.Version

import System.IO
import System.Exit

-- Return ExitSuccess if cabalfile given as cmdline arg 1 depends on
-- package arg 2 of version arg 3

main = do
  verStr <- getContents
  let lines' = lines verStr
  let vers = sort $ map parseVersion (filter (/=[]) $ lines')
  mapM (putStrLn.display) vers
  exitSuccess

parseVersion :: String -> Version
parseVersion= fromJust . simpleParse

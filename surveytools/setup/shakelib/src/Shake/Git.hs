module Shake.Git where

import Control.Monad

import Development.Shake
import Development.Shake.FilePath
-- import System.Process

type Url = String

data GitRepo = GitRepo FilePath Url
 -- deriving stuff

requireGit :: GitRepo -> Rules ()
requireGit (GitRepo fp url) = do
  fp *> ( \_ -> do
    alwaysRerun
    repoDirExists <- doesFileExist $ takeDirectory fp
    when (not repoDirExists) $ do
      systemCwd (takeDirectory fp) "git" ["clone", url]
    systemCwd fp "git" ["pull"]
    )

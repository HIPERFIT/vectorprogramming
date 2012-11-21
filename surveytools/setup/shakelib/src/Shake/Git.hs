 {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Shake.Git where

import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Monad
import Control.Monad.IO.Class

import Data.Binary
import Data.Binary.Generic

import Data.Data
import Data.Generics as DG

import GHC.Generics as GG

import Data.Hashable.Generic
import Data.Typeable

import Development.Shake
import Development.Shake.FilePath

import System.Exit
import System.Process

type Url = String

-- | A Git repository is described @GitRepo localRepoDir expectedFiles remoteUrl@.
data GitRepo = GitRepo FilePath [FilePath] Url
  deriving (Data, Eq, GG.Generic, Show, Typeable)

instance NFData GitRepo where
  rnf = genericRnf

instance Binary GitRepo where
  put = putGeneric
  get = getGeneric

instance Hashable GitRepo where
  hashWithSalt = gHashWithSalt

instance Rule GitRepo () where
  validStored (GitRepo _ _ _) time = do
    -- Git repositories older than an hour are no longer valid
    -- (future work)
    return False

gitUpdateRule :: Rules ()
gitUpdateRule  = do
  -- if anything is required inside a git repo, assume the repo contains it
  rule $ \(GitRepo fp _ url) -> Just $ do
    repoDirExists <- systemTest $ "test -d " ++ fp </> ".git"
    when (not repoDirExists) (do
      systemCwd (takeDirectory fp) "git" ["clone", url, fp])
    systemCwd fp "git" ["pull"]

-- | Add shake rules for all the expected files in a git repo to require the gitUpdate
gitFilesRule :: GitRepo -> Rules ()
gitFilesRule (repo@(GitRepo repoDir fs _)) =
  (map (repoDir </>) fs) **> ( \_ -> requireGit [repo])

-- | State that a repository is required
requireGit :: [GitRepo] -> Action ()
requireGit repos = do
  apply repos
  return ()
  -- (map (\ (GitRepo _ fs _) -> fs) repos)
  --

-- Run system command, and return True if ExitSuccess, False otherwise
systemTest :: MonadIO m => String -> m Bool
systemTest cmd = liftIO $ do
  res <- system cmd
  case res of
    ExitSuccess -> return True
    _ -> return False

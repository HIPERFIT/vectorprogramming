 {-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ShakeLib.Git where

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

type Name = String
type Url = String

data Branch = Branch Name Revision
  deriving (Data, Eq, GG.Generic, Show, Typeable)

data Revision = Commit String | Head | RevAsIs
  deriving (Data, Eq, GG.Generic, Show, Typeable)

instance NFData Branch where
  rnf = genericRnf

instance Binary Branch where
  put = putGeneric
  get = getGeneric

instance Hashable Branch where
  hashWithSalt = gHashWithSalt

instance NFData Revision where
  rnf = genericRnf

instance Binary Revision where
  put = putGeneric
  get = getGeneric

instance Hashable Revision where
  hashWithSalt = gHashWithSalt

-- | A Git repository is described @GitRepo branchName localRepoDir expectedFiles remoteUrl@.
data GitRepo = GitRepo Branch FilePath [FilePath] Url
  deriving (Data, Eq, GG.Generic, Show, Typeable)

instance NFData GitRepo where
  rnf = genericRnf

instance Binary GitRepo where
  put = putGeneric
  get = getGeneric

instance Hashable GitRepo where
  hashWithSalt = gHashWithSalt

instance Rule GitRepo () where
  validStored (GitRepo _ _ _ _) time = do
    -- Git repositories older than an hour are no longer valid
    -- (future work)
    return False

gitUpdateRule :: GitRepo ->  Rules ()
gitUpdateRule repo = do
  rule $ \repo'@(GitRepo (Branch branchNm rev) fp _ url) -> 
    if repo == repo'
      then
        Just $ do
        repoDirExists <- systemTest $ "test -d " ++ fp </> ".git"
        when (not repoDirExists) (do
          systemCwd (takeDirectory fp) "git" ["clone", url, takeFileName fp])
        systemCwd fp "git" ["checkout", branchNm]
        case rev of
          Head -> do
            systemCwd fp "git" ["pull"]
          Commit c -> do
            systemCwd fp "git" ["checkout", c]
          RevAsIs -> return ()
      else
        Nothing

-- | Add shake rules for all the expected files in a git repo to require the gitUpdate
gitFilesRule :: GitRepo -> Rules ()
gitFilesRule (repo@(GitRepo _ repoDir fs _)) =
  (map (repoDir </>) fs) **> ( \_ -> requireGit [repo])

-- | State that a repository is required
requireGit :: [GitRepo] -> Action ()
requireGit repos = do
  apply repos
  return ()
  -- (map (\ (GitRepo _ fs _) -> fs) repos)
  --

-- * common GitRepo constructors

gitRepo :: FilePath -> Url -> GitRepo
gitRepo fp url = GitRepo (Branch "master" Head) fp [] url

gitRepoFiles :: FilePath -> [FilePath] -> Url -> GitRepo
gitRepoFiles fp fs url = GitRepo (Branch "master" Head) fp fs url

-- * Aux definitions

-- Run system command, and return True if ExitSuccess, False otherwise
systemTest :: MonadIO m => String -> m Bool
systemTest cmd = liftIO $ do
  res <- system cmd
  case res of
    ExitSuccess -> return True
    _ -> return False

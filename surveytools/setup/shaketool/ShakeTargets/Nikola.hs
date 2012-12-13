module ShakeTargets.Nikola(rules, nikolaCabal) where

import Control.Monad(when)

import Development.Shake
import Development.Shake.FilePath
import ShakeLib.Cabal
import ShakeLib.Git

cudaGit  = gitRepoFiles "cuda" ["cuda.cabal"] "git://github.com/tmcdonell/cuda.git"
cudaMainlandGit  = gitRepoFiles "cuda-mainland" ["cuda.cabal"] "git://github.com/mainland/cuda.git"
nikolaGit = --gitRepoFiles "nikola" ["nikola.cabal"]
  -- "git://github.com/mainland/nikola.git"
  GitRepo (Branch "hiperfit" Head) "nikola" ["nikola.cabal"] "git://github.com/HIPERFIT/nikola.git"

cudaMainlandCabal =   CabalFile "cuda-mainland/cuda.cabal" "cuda"
              []
               -- ["--extra-include-dirs=/usr/local/cuda/include/"]
               -- ["--extra-include-dirs=/usr/local/cuda-5.0/include/"]

cudaCabal =   CabalFile "cuda/cuda.cabal" "cuda"
               -- ["--extra-include-dirs=/usr/local/cuda/include/"]
               ["--extra-include-dirs=/usr/local/cuda-5.0/include/"]

nikolaCabal = CabalFile "nikola/nikola.cabal" "nikola" []
            {-   ["--disable-library-profiling",
               "--enable-tests",
--               "--flags=examples",
               "--configure-option=--with-nvcc=/usr/local/cuda-5.0/bin/nvcc"] -}

gitRepos = [cudaGit, nikolaGit]
cabalPkgs = [cudaCabal, nikolaCabal]

rules = do
  mapM gitUpdateRule gitRepos
  mapM gitFilesRule gitRepos

  [nikolaCabal,cudaCabal]
    `hsPkgsRule` (\pkg -> do
                            when (pkg == nikolaCabal) (requireCabal [cudaCabal])
                            cabalClean pkg
                            cabalAutoconf pkg
                            buildCabal pkg)


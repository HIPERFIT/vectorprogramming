module Main where

import Development.Shake
import Development.Shake.FilePath
import Shake.Cabal
import Shake.Git

cudaGit  = gitRepoFiles "cuda" ["cuda.cabal"] "git://github.com/tmcdonell/cuda.git"
-- GitRepo (Branch "plc-tmp" RevAsIs) "cuda" ["cuda.cabal"]
--              "git://github.com/tmcdonell/cuda.git"
nikolaGit = gitRepoFiles "nikola" ["nikola.cabal"]
  "git://github.com/mainland/nikola.git"

gitRepos = [cudaGit, nikolaGit]

cudaCabal =   CabalFile "cuda/cuda.cabal" "cuda"
               -- ["--extra-include-dirs=/usr/local/cuda/include/"]
               ["--extra-include-dirs=/usr/local/cuda-5.0/include/"]

nikolaCabal = CabalFile "nikola/nikola.cabal" "nikola" []
            {-   ["--disable-library-profiling",
               "--enable-tests",
--               "--flags=examples",
               "--configure-option=--with-nvcc=/usr/local/cuda-5.0/bin/nvcc"] -}

cabalPkgs = [cudaCabal, nikolaCabal]

main = shake shakeOptions $ do
  gitUpdateRule
  mapM gitFilesRule gitRepos

  -- buildCabalRule [... normal builds here...]
  -- These two should be generalised into one
  [nikolaCabal,cudaCabal]
    `hsPkgsRule` (\pkg -> do
                            systemCwd (takeDirectory $ cabalFile pkg) "cabal" ["clean"]
                            systemCwd (takeDirectory $ cabalFile pkg) "autoconf" []
                            buildCabal pkg)

  action $ do
    need $ map cabalFile cabalPkgs
    -- requireCabal cabalPkgs -- insufficient, as the order does indeed matter..
    requireCabal [cudaCabal]
    requireCabal [nikolaCabal]

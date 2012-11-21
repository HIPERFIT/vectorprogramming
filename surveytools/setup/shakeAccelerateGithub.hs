module Main where

import Development.Shake
import Shake.Cabal
import Shake.Git

cudaGit  =     GitRepo "cuda" ["cuda.cabal"] "git://github.com/tmcdonell/cuda.git"
accelGit =     GitRepo "accelerate" ["accelerate.cabal"] "git://github.com/AccelerateHS/accelerate.git"
accelIoGit =   GitRepo "accelerate-io" ["accelerate-io.cabal"] "git://github.com/AccelerateHS/accelerate-io.git"
accelCudaGit = GitRepo "accelerate-cuda" ["accelerate-cuda.cabal"] "git://github.com/AccelerateHS/accelerate-cuda.git"

gitRepos = [cudaGit, accelGit, accelIoGit, accelCudaGit]

cudaCabal =      CabalFile "cuda/cuda.cabal" "cuda" [ "--extra-include-dirs=/usr/local/cuda/include/"]
accelCabal =     CabalFile "accelerate/accelerate.cabal" "accelerate" []
accelIoCabal =   CabalFile "accelerate-io/accelerate-io.cabal" "accelerate-io" []
accelCudaCabal = CabalFile "accelerate-cuda/accelerate-cuda.cabal" "accelerate-cuda"
                  ["--extra-include-dirs=/usr/local/cuda/include/",
                    "--extra-lib-dirs=/usr/local/cuda/lib64/"]

cabalPkgs = [cudaCabal, accelCabal, accelIoCabal, accelCudaCabal]

main = shake shakeOptions $ do
  gitUpdateRule
  mapM gitFilesRule gitRepos

  buildCabalRule [accelCabal, accelIoCabal, accelCudaCabal]
  cudaCabal `hsPkgRule` (\pkg -> do
                                  systemCwd "cuda" "autoconf" []
                                  buildCabal pkg
                                  )
  action $ do
    need $ map cabalFile cabalPkgs
    -- requireCabal cabalPkgs -- insufficient, as the order does indeed matter..
    requireCabal [cudaCabal]
    requireCabal [accelCabal]
    requireCabal [accelIoCabal]
    requireCabal [accelCudaCabal]

module Main where

import Development.Shake
import Shake.Cabal
import Shake.Git

main = shake shakeOptions $ do
  requireGit (GitRepo "accelerate" "git://github.com/AccelerateHS/accelerate.git")
  requireGit (GitRepo "accelerate-cuda" "git://github.com/AccelerateHS/accelerate-cuda.git")
  requireGit (GitRepo "accelerate" "git://github.com/AccelerateHS/accelerate-io.git")

  requireCabal (CabalFile "accelerate/accelerate.cabal" [])
  requireCabal (CabalFile "accelerate-io/accelerate-io.cabal" [])
  requireCabal (CabalFile "accelerate-cuda/accelerate-cabal.cabal"
    [ "--extra-include-dirs=/usr/local/cuda/include/",
      "--extra-lib-dirs=/usr/local/cuda/lib64/"])

module ShakeTargets.AccelerateGithub(rules, accelCudaCabal, accelIoCabal) where

import Development.Shake
import Development.Shake.FilePath
import ShakeLib.Cabal
import ShakeLib.Git

cudaGit  =     GitRepo (Branch "plc-tmp" Head) "cuda" ["cuda.cabal"] "git://github.com/plcplc/cuda.git"
accelGit =     gitRepoFiles "accelerate" ["accelerate.cabal"] "git://github.com/AccelerateHS/accelerate.git"
accelIoGit =   gitRepoFiles "accelerate-io" ["accelerate-io.cabal"] "git://github.com/AccelerateHS/accelerate-io.git"
accelCudaGit = GitRepo (Branch "master" -- (Commit "99b82e4ad8887338c39b3be07ed7b1f7f61c20fb"))
                                        (Commit "e8d131c34ad46fed1274451c9656136034b55023"))
                 "accelerate-cuda" ["accelerate-cuda.cabal"]
                 "git://github.com/AccelerateHS/accelerate-cuda.git"

gitRepos = [cudaGit, accelGit, accelIoGit, accelCudaGit]

cudaCabal =      CabalFile "cuda/cuda.cabal" "cuda" [ "--extra-include-dirs=/usr/local/cuda/include/"]
accelCabal =     CabalFile "accelerate/accelerate.cabal" "accelerate" []
accelIoCabal =   CabalFile "accelerate-io/accelerate-io.cabal" "accelerate-io" []
accelCudaCabal = CabalFile "accelerate-cuda/accelerate-cuda.cabal" "accelerate-cuda"
                  ["--extra-include-dirs=/usr/local/cuda/include/",
                    "--extra-lib-dirs=/usr/local/cuda/lib64/"]

cabalPkgs = [cudaCabal, accelCabal, accelIoCabal, accelCudaCabal]

rules = do
  mapM gitUpdateRule gitRepos
  mapM gitFilesRule gitRepos

  buildCabalRule [accelCabal]

  accelIoCabal `hsPkgRule`
    (\pkg -> do
      requireCabal [accelCabal]
      buildCabal pkg)

  accelCudaCabal `hsPkgRule`
    (\pkg -> do
      requireCabal [cudaCabal, accelCabal]
      cabalClean pkg
      cabalAutoconf pkg
      buildCabal pkg
      )

  cudaCabal `hsPkgRule`
    (\pkg -> do
      cabalClean pkg
      cabalAutoconf pkg
      buildCabal pkg)

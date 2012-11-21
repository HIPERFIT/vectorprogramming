module Shake.Cabal where

import Development.Shake
import Development.Shake.FilePath

type Args = [String]

data CabalPackage =
  CabalFile FilePath Args
  | CabalHackage String Args

requireCabal :: CabalPackage -> Rules ()

requireCabal (CabalFile fp args) = action $ do
  need [fp]
  alwaysRerun
  (out,_) <- systemOutput "ghc-pkg" ["--simple-output","list", takeBaseName fp]
  if wc_l out < 1 then
    system' "cabal" $ args ++ ["install"]
    else return ()
requireCabal (CabalHackage pkg args) = action $ do
  alwaysRerun
  (out,_) <- systemOutput "ghc-pkg" ["--simple-output","list", pkg]
  if wc_l out < 1 then
    system' "cabal" $ args ++ ["install", pkg]
    else return ()

wc_l = length . lines

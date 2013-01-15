{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ShakeLib.Cabal where

import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Monad

import Data.Binary
import Data.Binary.Generic

import Data.Data
import Data.Generics as DG

import GHC.Generics as GG

-- import Data.Hashable
import Data.Hashable.Generic
import Data.Typeable

import Development.Shake
import Development.Shake.FilePath

import System.Process

type Args = [String]
type Version = String

-- | Type modelling packages installable by Cabal.
data CabalPackage =
    CabalFile FilePath String Args
  | CabalHackage String Args
  deriving (Data, Eq, GG.Generic, Show, Typeable)

instance NFData CabalPackage where
  rnf = genericRnf

instance Binary CabalPackage where
  put = putGeneric
  get = getGeneric

instance Hashable CabalPackage where
  hashWithSalt = gHashWithSalt

cabalPackageName (CabalFile _ nm _) = nm
cabalPackageName (CabalHackage nm _) = nm

isCabalFile (CabalFile _ _ _) = True
isCabalFile _ = False

cabalFile (CabalFile file _ _) = file
cabalFile (CabalHackage nm _) = error "CabalHackage has no defined cabal file!"

instance Rule CabalPackage (Version) where
  -- Retrieve the stored version
  storedValue pkg = do
    out <- readProcess "ghc-pkg" ["--simple-output","list", cabalPackageName pkg] ""
    case lines out of
     -- extract currently installed version
     hd:_ -> do
       return $ Just hd
     _ -> return Nothing

-- | Define a rule to build a haskell package
hsPkgRule :: CabalPackage -> (CabalPackage -> Action ()) -> Rules ()
hsPkgRule pkg act = rule $ \ pkg' ->
  if pkg == pkg'
    then Just $ do
      -- If ghc-pkg doesn't already think this is installed, go ahead:
      installed <- ghcPkgList pkg
      if length installed == 0 then do
        act pkg'
        -- Check what was actually installed
        --(out,_) <- systemOutput "ghc-pkg" ["--simple-output","list", cabalPackageName pkg]
        pkgsIns <- ghcPkgList $ pkg
        case pkgsIns of
          [] -> error $ "hsPkg rule action didn't install " ++ cabalPackageName pkg ++ " as promised!"
          hd:_ -> do
            let ver = ghcPkgVer hd
            putQuiet $ "Action installed \"" ++ hd ++ "\", recorded version = \"" ++ ver ++ "\""
            return ver
        else return $ ghcPkgVer (head installed)

    else Nothing

hsPkgsRule :: [CabalPackage] -> (CabalPackage -> Action ()) -> Rules ()
hsPkgsRule pkgs act = mapM_ (`hsPkgRule` act) pkgs

-- | rule to just use @buildCabal@ to build a list of haskell packages.
buildCabalRule :: [CabalPackage] -> Rules ()
buildCabalRule pkgs = pkgs `hsPkgsRule` buildCabal

-- | require haskell packages
requireCabal :: [CabalPackage] -> Action ()
requireCabal pkgs = do
  need $ map cabalFile pkgs
  apply pkgs >> return ()

-- | Build a package from using cabal. Remember to @need@ all required files.
buildCabal :: CabalPackage -> Action ()
buildCabal (CabalFile fp nm args) = do
  -- maybe give cabal file explicitly : nm <.> "cabal"
  systemCwd (takeDirectory fp) "cabal" $ args ++ ["install"]
  return ()
buildCabal (CabalHackage pkg args) = do
  system' "cabal" $ args ++ ["install", pkg]
  return ()

cabalClean pkg = systemCwd (takeDirectory $ cabalFile pkg) "cabal" ["clean"]
cabalAutoconf pkg = systemCwd (takeDirectory $ cabalFile pkg) "autoconf" []
-- * Aux definitions

ghcPkgList :: CabalPackage -> Action [String]
ghcPkgList pkg = do
  (out,_) <- systemOutput "ghc-pkg" ["--simple-output","list", cabalPackageName pkg]
  return $ lines out

-- | Turn a name-ver to ver
ghcPkgVer :: String -> String
ghcPkgVer ver = reverse $ takeWhile (/='-')  (reverse ver)

wc_l = length . lines


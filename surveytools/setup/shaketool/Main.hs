module Main where

import Development.Shake
import ShakeLib.Cabal

import qualified ShakeTargets.AccelerateGithub as A
import qualified ShakeTargets.Nikola as N

import System.Environment(getArgs)

validArgs = ["--nikola", "--accelerate"]

usage = "usage : " ++ show validArgs

main = do 
  args <- getArgs
  case args of
    [arg] ->
      shake shakeOptions $ do
        case arg of
          "--accelerate" -> do
              A.rules
              action $ requireCabal [A.accelIoCabal, A.accelCudaCabal]

          "--nikola" -> do
              N.rules
              action $ requireCabal [N.nikolaCabal]
    _ -> putStrLn usage

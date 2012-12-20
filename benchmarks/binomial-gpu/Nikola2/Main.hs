module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import Data.Array.Nikola.Backend.CUDA (initializeCUDACtx)
import qualified American.Nikola as AMN


main = do
  initializeCUDACtx
  putStrLn "OK"
  execute AMN.binomCompiled


execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

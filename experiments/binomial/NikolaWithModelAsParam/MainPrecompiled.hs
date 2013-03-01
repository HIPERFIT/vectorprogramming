module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import qualified Data.Array.Nikola.Backend.CUDA as C
import American
import System.IO

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  C.initializeCUDACtx
  putStrLn "OK"
  execute (binomRun $ binomCompiled defaultModel)

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

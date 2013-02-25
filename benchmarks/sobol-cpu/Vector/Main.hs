module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import System.IO
import Sobol

import Control.DeepSeq(($!!), NFData(..))

main = do
  -- We're not connected to a terminal, so we need to hFlush by default.
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  putStrLn "OK" -- no preparation steps
  execute sobolSequence

execute :: (NFData b, Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show $!! f . read $ str)

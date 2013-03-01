module Main where

import Control.Monad (when, forever)
import System.Exit (exitSuccess)
import System.IO
import LSM

import Control.DeepSeq(($!!), NFData(..))

main = do
  -- We're not connected to a terminal, so we need to hFlush by default.
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  putStrLn "OK" -- no preparation steps
  execute (lsm n_points)

execute :: (NFData b, Read a, Show b) => (a -> IO b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  e <- f . read $ str
  putStrLn $ "RESULT " ++ show e

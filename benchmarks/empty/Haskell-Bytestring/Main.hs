{-# LANGUAGE MagicHash #-}
module Main where
import Control.Monad (when, forever)
import System.Exit (exitSuccess)

import System.Environment(getArgs)
import System.IO

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS

main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "OK" -- no preparation steps
  execute

execute :: IO ()
execute = do
  exit <- BS.unsafePackAddressLen 4 "EXIT"#
  ok <- BS.unsafePackAddressLen 2 "EXIT"#
  result <- BS.unsafePackAddressLen 11 "RESULT 42.0"#
  forever $ do
    str <- BS.getLine
    when (str == exit) (BS.putStrLn ok >> exitSuccess)
    BS.putStrLn result

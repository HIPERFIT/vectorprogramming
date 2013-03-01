module Main where

import qualified Data.Array.Nikola.Backend.CUDA as C
import American

main = do
  C.initializeCUDACtx
  putStrLn "OK"
  execute (binomRun $ binom defaultModel)

execute :: (Read a, Show b) => (a -> b) -> IO ()
execute f = forever $ do
  str <- getLine
  when (str == "EXIT") (putStrLn "OK" >> exitSuccess)
  putStrLn $ "RESULT " ++ (take 150 . show . f . read $ str)

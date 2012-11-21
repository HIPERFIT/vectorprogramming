module Main where

import System.Environment (getArgs)

import qualified Data.Array.Repa as R

import MonteCarloPricing
import qualified PricingExample2 as PE1

main :: IO()
main = do args <- getArgs
          let n = if null args then 100000 else read (head args)
              conf = PE1.example_init n -- all examples should export this name
              ----------------------
              l = map (R.toList . sobolInd conf) [1..100]
          print $ map (toRational . head) l
--              res    = mc_pricing conf
--              resopt = tiledSkeleton conf 32 (mc_pricing_chunk conf)
          putStrLn ("Config: " ++ show n ++ " iterations")
--          putStrLn ("Computed opt: " ++ show resopt)
--          putStrLn ("Computed:     " ++ show res)

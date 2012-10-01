module Main where

import American
import BinomialTest (runTest)

main = runTest binom'
  where binom' n = binomCompiled $ defaultModel {bankDays = n}

module Main where

import Sobol
import BenchmarkRunner.Main(runTest)

main = runTest sobolSequence

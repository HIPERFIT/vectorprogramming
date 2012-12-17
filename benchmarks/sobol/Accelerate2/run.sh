#!/bin/bash
HSENV_=accelerate-github-GHC7.6.1
buildBenchmark $HSENV_
runBenchmark $HSENV_ benchmark-sobol-Accelerate2

#!/bin/bash

HSENV_=vanilla-GHC7.4.2
buildBenchmark $HSENV_
runBenchmark $HSENV_ benchmark-sobol-Vector

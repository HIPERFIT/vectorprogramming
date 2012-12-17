#!/bin/bash
HSENV_=nikola-GHC7.4.2
buildBenchmark $HSENV_
runBenchmark $HSENV_ benchmark-sobol-Nikola

DIKU Master thesis
==================

GPUs, DSLs, vectorization, financial applications and functional programming.


Directory organisation
======================

benchmarks  : executable experiments.
 - [experiment0]
   - [language0]
   - [language1]
   - ...
 - [experiment1]
   - [language0]
   - [language1]
   - ...
 - ...
 
experiments : every case starts here. Can be every kind of experiment

surveytools : scripts for gathering survey data

results     : output from surveytools

build       : build files for surveytools
 - hsenvs   : Local GHC installations
 - ghcs     : Cache of GHC installation files

tex         : reporting

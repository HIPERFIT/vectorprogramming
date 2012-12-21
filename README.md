DIKU Master thesis
==================

GPUs, DSLs, vectorization, financial applications and functional programming.


Directory organisation
======================

* /benchmarks  : executable experiments.
  - /<experiment0>
    - /<insance0>
      - /run.sh
      - /setup.sh
    - /...
  - /<instance1>
    - /...
  - /<experiment1>
    - /...

* /experiments - every case starts here. Can be every kind of experiment

* /surveytools - scripts for gathering survey data. See [the wiki page](vectorprogramming/wiki/Surveytools).

* /results -output from surveytools

* /build - build files for surveytools
  - /hsenvs - Local GHC installations
  - /ghcs - Cache of GHC installation files
  - /logs - Logfiles from all scripts, i.e. installation/setup and benchmark running.


* tex - report text and code

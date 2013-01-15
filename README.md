DIKU Master thesis
==================

GPUs, DSLs, vectorization, financial applications and functional programming.


Directory organisation
======================

* `/benchmarks` - Executable benchmarks. May contain multiple experiments.
* `/benchmarks/<experiment0>/` - Experiment folder. May contain multiple instances.
* `/benchmarks/<experiment0>/inputs` - Experiment arguments, with each lexical word corresponding to a particular benchmark.
* `/benchmarks/<experiment0>/plot_baseline` - Argument for benchmark result plotting.
* `/benchmarks/<experiment0>/<insance0>/run.sh` - Instance execution entry point.
* `/benchmarks/<experiment0>/<insance0>/setup.sh` - Instance setup.
* `/build` - Build files for surveytools
* `/build/hsenvs` - Local GHC installations
* `/build/ghcs` - Cache of GHC installation files
* `/build/logs` - Logfiles from all scripts, i.e. installation/setup and benchmark running.
* `/experiments` - Free form working scratchpad. Every piece of work initially resides here, and may later be promoted to a *benchmark* or a *case study*.
* `/results` - Output from surveytools
* `/surveytools` - Scripts for gathering survey data. See [the wiki page](vectorprogramming/wiki/Surveytools).
* `/sourceme.sh` - A script to set up the interactive shell to be aware of project commands and environment.
* `/tex` - report text and code

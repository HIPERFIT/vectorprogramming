DIKU Master thesis
==================

GPUs, DSLs, vectorization, financial applications and functional programming.


Directory organisation
======================

- `/benchmarks` - executable experiments.
  - `/<experiment0>`
    - `inputs` - experiment arguments, with each lexical word corresponding to a particular benchmark.
    - `plot_baseline` - argument for benchmark result plotting.
    - `/<insance0>`
      - `/run.sh` - instance execution entry point.
      - `/setup.sh` - instance setup.
    - `/...`

  - `/<instance1>`
    - `/...`

  - `/<experiment1>`
    - `/...`

- `/experiments` - Free form working scratchpad. Every piece of work initially resides here, and may later be promoted to a *benchmark* or a *case study*.

- `/surveytools` - Scripts for gathering survey data. See [the wiki page](vectorprogramming/wiki/Surveytools).

- `/results` - Output from surveytools

- `/build` - Build files for surveytools
  - `/hsenvs` - Local GHC installations
  - `/ghcs` - Cache of GHC installation files
  - `/logs` - Logfiles from all scripts, i.e. installation/setup and benchmark running.

- `/tex` - report text and code

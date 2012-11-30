#!/bin/bash

# Hoop up with CUDA
export LD_LIBRARY_PATH="/usr/local/cuda-5.0/lib64"
export PATH="$PATH:/usr/local/cuda-5.0/bin"

# usage: $0 programname hsenv logtag

# make every non-zero exitcode fatal
set -e

# fetching arguments

BENCHMARK=$1
PLATFORM=$2

# Name of benchmark (for naming logs, reports etc.)
NAME=$BENCHMARK-$PLATFORM

BUILDROOT=$(readlink -f $(pwd)/../../build)
RESULTROOT=$(readlink -f $(pwd)/../../results)
HSENVROOT=$BUILDROOT/hsenvs
LOGDIR=$BUILDROOT/logs
mkdir -p $LOGDIR


# The main directory of the test program
PROGRAMDIR=$(readlink -f $(pwd)/../../benchmarks/$BENCHMARK/$PLATFORM)
# (note the underscore to avoid variable capture when sourcing!)
HSENV_=$3
LOGTAG=$4

SUMMARYDIR=$RESULTROOT/benchmark-summaries/$LOGTAG
SUMMARYFILE=$SUMMARYDIR/$NAME.csv
REPORTFILE=$SUMMARYDIR/$NAME-report.html

mkdir -p $SUMMARYDIR
LOGFILE=$LOGDIR/$LOGTAG-$BENCHMARK-$PLATFORM

(
source $HSENVROOT/$HSENV_/.hsenv_$HSENV_/bin/activate

# run configure and build
cd $PROGRAMDIR
cabal clean
cabal install --only-dependencies
cabal configure
cabal build

cd ..

deactivate_hsenv
) > $LOGFILE-Build

# run the result
cat ../../benchmarks/$BENCHMARK/inputs | $PROGRAMDIR/dist_$HSENV_/build/benchmark-${NAME}/benchmark-${NAME} \
  --summary $SUMMARYFILE \
  --output $REPORTFILE +RTS -N | tee $LOGFILE-Benchmark



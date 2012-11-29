#!/bin/bash

# usage: $0 programname hsenv logtag

# make every non-zero exitcode fatal
set -e

# fetching arguments


BENCHMARK=$1
PLATFORM=$2

# Name of benchmark (for naming logs, reports etc.)
NAME=$BENCHMARK-$PLATFORM

# The main directory of the test program
PROGRAMDIR=$(readlink -f $(pwd)../../benchmarks/$BENCHMARK/$PLATFORM)
# (note the underscore to avoid variable capture when sourcing!)
HSENV_=$3
LOGTAG=$4
HSENVROOT=$(readlink -f $(pwd)/../../build/hsenvs)

echo $PROGRAMDIR
exit 0

SUMMARYDIR=summaries/$LOGTAG
SUMMARYFILE=$SUMMARYDIR/$NAME.csv
REPORTFILE=$SUMMARYDIR/$NAME-samples.csv

# summarydir created by Main.hs
#mkdir -p $SUMMARYDIR

mkdir -p logs
LOGFILE=logs/$LOGTAG-$BENCHMARK-$PLATFORM

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



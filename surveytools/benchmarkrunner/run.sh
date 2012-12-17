#!/bin/bash

# Hook up with CUDA
export LD_LIBRARY_PATH="/usr/local/cuda-5.0/lib64"
export PATH="$PATH:/usr/local/cuda-5.0/bin"

# usage: $0 programname hsenv logtag

# make every non-zero exitcode fatal
set -e

# fetching arguments
BENCHMARK=$1
PLATFORM=$2
LOGTAG=$3

# Name of benchmark (for naming logs, reports etc.)
NAME=$BENCHMARK-$PLATFORM

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`
ROOT=$(readlink -f ${SCRIPTROOT}/../..)

BUILDROOT=$ROOT/build
BENCHROOT=$ROOT/benchmarks
RESULTROOT=$ROOT/results
HSENVROOT=$BUILDROOT/hsenvs
LOGDIR=$BUILDROOT/logs
mkdir -p $LOGDIR

# The main directory of the test program
PROGRAMDIR=$BENCHROOT/$BENCHMARK/$PLATFORM
# (note the underscore to avoid variable capture when sourcing!)

SUMMARYDIR=$RESULTROOT/benchmark-summaries/$LOGTAG
SUMMARYFILE=$SUMMARYDIR/$NAME.csv
REPORTFILE=$SUMMARYDIR/$NAME-report.html

mkdir -p $SUMMARYDIR
LOGFILE=$LOGDIR/$LOGTAG-$BENCHMARK-$PLATFORM

buildBenchmark() {

  HSENV_=$1
  (
    source $HSENVROOT/$HSENV_/.hsenv_$HSENV_/bin/activate
    cabal clean
    cabal install --only-dependencies
    cabal configure
    cabal build
    deactivate_hsenv
  ) > $LOGFILE-Build

}

runBenchmark() {

  HSENV_=$1
  #PKGNAME=$(sed -n -e '/name:/p' *.cabal |head -n 1 |gawk '{ print $2}')
  PROGRAM=$2

  echo running!
  cat $BENCHROOT/$BENCHMARK/inputs | dist_$HSENV_/build/$PROGRAM/$PROGRAM \
    --summary $SUMMARYFILE \
    --output $REPORTFILE +RTS -N | tee -a $LOGFILE-Benchmark

}

echo $PROGRAMDIR/run.sh

# Actually run the benchmark
(
  cd $PROGRAMDIR
  source run.sh
) > $LOGFILE-Run

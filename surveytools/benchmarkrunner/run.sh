#!/bin/bash

# Hook up with CUDA
export LD_LIBRARY_PATH="/usr/local/cuda-5.0/lib64"
export PATH="$PATH:/usr/local/cuda-5.0/bin"

# make every non-zero exitcode fatal
set -e

# fetching arguments
BENCHMARK=$1
INSTANCE=$2
LOGTAG=$3

# Name of benchmark (for naming logs, reports etc.)
NAME=$BENCHMARK-$INSTANCE

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
PROGRAMDIR=$BENCHROOT/$BENCHMARK/$INSTANCE
# (note the underscore to avoid variable capture when sourcing!)

SUMMARYDIR=$RESULTROOT/benchmark-summaries/$LOGTAG
SUMMARYFILE=$SUMMARYDIR/$NAME.csv
REPORTFILE=$SUMMARYDIR/$NAME-report.html

RUNNER=$SCRIPTROOT/dist_*/build/benchmarkunner/benchmarkunner

mkdir -p $SUMMARYDIR
LOGFILE=$LOGDIR/$LOGTAG-$BENCHMARK-$INSTANCE

# A useful environment.
source $ROOT/surveytools/bin/loadHSENV

# Setup the benchmark
(
  cd $PROGRAMDIR
  source setup.sh
) 2>&1| tee $LOGFILE-Setup

# Actually run the benchmark
(
  loadHSENV "buildtools"
  cabal install --only-dependencies
  cabal configure
  cabal build
  deactivate_hsenv
)

echo running $PROGRAMDIR/run.sh
cat $BENCHROOT/$BENCHMARK/inputs | $RUNNER run.sh \
  --summary $SUMMARYFILE \
  --output $REPORTFILE +RTS -N1 2>&1| tee $LOGFILE-Run


#!/bin/bash

# Hook up with CUDA
export LD_LIBRARY_PATH="/usr/local/cuda-5.0/lib64"
export PATH="$PATH:/usr/local/cuda-5.0/bin"

# make every non-zero exitcode fatal
set -e

# fetching arguments
BENCHMARK="$1"
INSTANCE="$2"
LOGTAG="$3"
#echo $INSTANCE
# Name of benchmark (for naming logs, reports etc.)
NAME="$INSTANCE"
#NAME=$BENCHMARK-$INSTANCE

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`
ROOT=$(readlink -f ${SCRIPTROOT}/../..)

BUILDROOT=$ROOT/build
BENCHROOT=$ROOT/benchmarks-paper
RESULTROOT=$ROOT/results
HSENVROOT=$BUILDROOT/hsenvs
LOGDIR=$BUILDROOT/logs/$LOGTAG
mkdir -p $LOGDIR/
LOGFILE=$LOGDIR/$BENCHMARK-$INSTANCE

# The main directory of the test program
PROGRAMDIR="$BENCHROOT/$BENCHMARK/$INSTANCE"
# (note the underscore to avoid variable capture when sourcing!)

SUMMARYDIR=$RESULTROOT/benchmark-summaries/$LOGTAG/$BENCHMARK/data
SUMMARYFILE=$SUMMARYDIR/$NAME.csv
REPORTFILE=$SUMMARYDIR/$NAME-report.html

RUNNER=$SCRIPTROOT/dist_*/build/benchmarkunner/benchmarkunner

mkdir -p "$SUMMARYDIR"

# Useful environments:
source $ROOT/surveytools/bin/loadHSENV

buildCabal() {
  # What you want most of the time when building benchmarks via cabal
  cabal install --only-dependencies
  cabal configure
  cabal build
}

#echo $PROGRAMDIR
cd "$PROGRAMDIR"

# Setup the benchmark
(
  source setup.sh
) 2>&1| tee $LOGFILE-Setup

# Actually run the benchmark
TIMEOUTMINS=50
echo running "$PROGRAMDIR/run.sh"
#cat $BENCHROOT/$BENCHMARK/inputs | $RUNNER "$PROGRAMDIR/run.sh" \
cat "$BENCHROOT/$BENCHMARK/inputs" | timeout --foreground $(($TIMEOUTMINS*60)) $RUNNER "$PROGRAMDIR/run.sh" \
  --summary "$SUMMARYFILE" \
  --output "$REPORTFILE" +RTS -N1 2>&1| tee $LOGFILE-Run

# In the case of incomplete output (when the benchmark does not
# complete), this will make R's CSV-parser happy!
echo $'\n' >> "$SUMMARYFILE"
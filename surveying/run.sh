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
PROGRAMDIR=benchmarks/$BENCHMARK/$PLATFORM
# (note the underscore to avoid variable capture when sourcing!)
HSENV_=$3
LOGTAG=$4

# source local definition of HSENVROOT
hostname=`hostname`
conffile=$hostname-env.sh

if [ -f $conffile ]; then
  source ./$conffile
else
  echo "Please make file $conffile, containing HSENVROOT variable definition"
  exit -1
fi

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
$PROGRAMDIR/dist_$HSENV_/build/benchmark-${NAME}/benchmark-${NAME} --summary $SUMMARYFILE \
  --template samples.tpl --output $REPORTFILE +RTS -N | tee $LOGFILE-Benchmark


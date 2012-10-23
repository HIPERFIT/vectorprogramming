#!/bin/bash

# usage: $0 programname hsenv logtag

# make every non-zero exitcode fatal
set -e

# fetching arguments

# The main directory of the test program
PROGRAM=$1
# (note the underscore to avoid variable capture when sourcing!)
HSENV_=$2
LOGTAG=$3

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
SUMMARYFILE=$SUMMARYDIR/$PROGRAM.csv
REPORTFILE=$SUMMARYDIR/$PROGRAM-samples.csv

# summarydir created by Main.hs
#mkdir -p $SUMMARYDIR

LOGFILE=logs/$LOGTAG-$PROGRAM

(
source $HSENVROOT/$HSENV_/.hsenv_$HSENV_/bin/activate

# run configure and build
cd Instances/$PROGRAM
cabal clean
cabal install --only-dependencies
cabal configure
cabal build

cd ..

deactivate_hsenv
) > $LOGFILE-Build

# run the result
Instances/$PROGRAM/dist_$HSENV_/build/binomialbenchmark-${PROGRAM}/binomialbenchmark-${PROGRAM} --summary $SUMMARYFILE \
  --template samples.tpl --output $REPORTFILE +RTS -N | tee $LOGFILE-Benchmark



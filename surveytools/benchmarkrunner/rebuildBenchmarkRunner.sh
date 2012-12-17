#!/bin/bash

set -e

# (note the underscore to avoid variable capture when sourcing!)
LOGTAG=$1

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`
ROOT=$(readlink -f ${SCRIPTROOT}/../..)
BUILDROOT=$ROOT/build
HSENVROOT=$BUILDROOT/hsenvs
LOGDIR=$BUILDROOT/logs
mkdir -p $LOGDIR
LOGFILE=$LOGDIR/$LOGTAG-rebuild-benchmarkrunner

(
    cd $SCRIPTROOT
    for hsenv in $HSENVROOT/*
    do
      hsenvBase=$(basename $hsenv)
      source $HSENVROOT/$hsenvBase/.hsenv_$hsenvBase/bin/activate

      cabal update
      cabal clean
      cabal install --reinstall


      deactivate_hsenv
    done
) > $LOGFILE

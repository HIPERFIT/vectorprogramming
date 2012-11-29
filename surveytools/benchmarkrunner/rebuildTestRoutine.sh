#!/bin/bash

set -e

# (note the underscore to avoid variable capture when sourcing!)
HSENV_=$1
LOGTAG=$2

BUILDROOT=$(readlink -f $(pwd)/../../build)
HSENVROOT=$BUILDROOT/hsenvs
LOGDIR=$BUILDROOT/log
mkdir -p $LOGDIR
LOGFILE=$LOGDIR/$LOGTAG-rebuild-$HSENV_

(

 source $HSENVROOT/$HSENV_/.hsenv_$HSENV_/bin/activate

cabal update
cabal clean
cabal install --reinstall

cd ..

deactivate_hsenv
) > $LOGFILE



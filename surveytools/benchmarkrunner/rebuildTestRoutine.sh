#!/bin/bash

set -e

# (note the underscore to avoid variable capture when sourcing!)
HSENV_=$1
LOGTAG=$2

# source local definition of HSENVROOT
hostname=`hostname`
conffile=$hostname-env.sh

if [ -f $conffile ]; then
  source ./$conffile
else
  echo "Please make file $conffile, containing HSENVROOT variable definition"
  exit -1
fi

mkdir -p logs
LOGFILE=logs/$LOGTAG-rebuild-$HSENV_

(

 source $HSENVROOT/$HSENV_/.hsenv_$HSENV_/bin/activate

cabal update
cabal clean
cabal install --reinstall

cd ..

deactivate_hsenv
) > $LOGFILE

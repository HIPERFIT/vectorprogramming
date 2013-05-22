#!/bin/sh

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

exec $SCRIPTROOT/dist_*/build/binomial-nikola/binomial-nikola $@

#!/bin/sh
SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

exec $SCRIPTROOT/binomial
#!/bin/bash

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

exec $SCRIPTROOT/dist_*/build/binomial-accelerate/binomial-accelerate $@

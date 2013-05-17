#!/bin/bash

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

exec $SCRIPTROOT/dist_*/build/sobol-pi-accelerate/sobol-pi-accelerate $@

#!/bin/bash

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

exec $SCRIPTROOT/dist_*/build/tfp-pi-nikola/tfp-pi-nikola $@


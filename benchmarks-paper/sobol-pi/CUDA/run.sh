#!/bin/bash

SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

exec $SCRIPTROOT/src-cpp/SobolQRNG

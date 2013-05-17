#!/bin/sh
SCRIPTFILE=`readlink -f $0`
SCRIPTROOT=`dirname $SCRIPTFILE`

python -u $SCRIPTROOT/Main.py
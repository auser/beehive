#!/bin/bash -ex

echo "listening on $PORT in $RUN_DIRECTORY/www"
cd $RUN_DIRECTORY/www
PYTHON=`which python`
exec $PYTHON -m SimpleHTTPServer $PORT
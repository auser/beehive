#!/bin/bash -ex

echo "listening on $PORT in $RUN_DIR/www"
cd $RUN_DIR/www
PYTHON=`which python`
exec $PYTHON -m SimpleHTTPServer $PORT
#!/bin/bash

echo "listening on $PORT in $RUN_DIRECTORY/www"
cd $RUN_DIRECTORY/www
exec "python -m SimpleHTTPServer $PORT"

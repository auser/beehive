#!/bin/sh -e

echo "Starting rails '$NAME' in $RUN_DIR"
export RAILS_ENV=$1
export GEM_HOME=$RUN_DIR/.beehive_gem_home
export GEM_PATH=$RUN_DIR/.beehive_gem_home:`gem env path`
export PATH=$RUN_DIR/.beehive_gem_home/bin:$PATH


echo $$ > $PIDFILE

if [ -f "Isolate" ]; then
  ruby -rubygems -e 'require "isolate"; Isolate.now!'
  $GEM_PATH
fi

if [ -f start.sh ]; then
  echo "Using start.sh file"
  exec /bin/sh start.sh
else
  echo "./script/server -p $PORT"
  exec ./script/server -p $PORT
fi

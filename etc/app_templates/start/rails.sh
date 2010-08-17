#!/bin/sh -e

echo "Starting rails '$NAME' in $RUN_DIR"
mkdir -p .beehive_gem_home
export RAILS_ENV=production
export GEM_HOME=$RUN_DIR/.beehive_gem_home
export GEM_PATH=$RUN_DIR/.beehive_gem_home
export PATH=$PATH:$RUN_DIR/.beehive_gem_home/bin

echo $$ > $PIDFILE
if [ -f start.sh ]; then
  echo "Using start.sh file"
  exec /bin/sh start.sh
else
  exec ./script/server -p $PORT
fi
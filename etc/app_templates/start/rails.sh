#!/bin/sh -e

echo "Starting rails '$NAME' in $RUN_DIR"
export RAILS_ENV=$DEPLOY_ENV
export GEM_HOME=$RUN_DIR/.beehive_gem_home
export GEM_PATH=$RUN_DIR/.beehive_gem_home:`gem env path`
export PATH=$RUN_DIR/.beehive_gem_home/bin:$PATH

echo $$ > $PIDFILE

if [ -f beehive_start.sh ]; then
  echo "Using beehive_start.sh file"
  exec /bin/sh beehive_start.sh
elif [ -f beehive/start.sh ]; then
  echo "Using beehive/start.sh file"
  exec /bin/sh beehive/start.sh
else
  echo "./script/server -p $PORT -e $RAILS_ENV"
  exec ./script/server -p $PORT -e $RAILS_ENV
fi

#!/bin/sh -e

echo "Starting rails '$NAME' in $RUN_DIR"
export RAILS_ENV=production
export GEM_HOME=.beehive_gem_home
export GEM_PATH=.beehive_gem_home:`gem env path`
export PATH=$RUN_DIR/.beehive_gem_home/bin:$PATH

echo $$ > $PIDFILE
if [ -f start.sh ]; then
  echo "Using start.sh file"
  exec /bin/sh start.sh
else
  exec ./script/server -p $PORT
fi
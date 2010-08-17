#!/bin/sh -e

echo "Starting rails '$NAME'"
mkdir -p .beehive_gem_home
export RAILS_ENV=production
export GEM_HOME=.beehive_gem_home
export GEM_PATH=.beehive_gem_home


echo $$ > $PIDFILE
if [ -f config.ru ]; then
  exec ./script/server -p $PORT
fi
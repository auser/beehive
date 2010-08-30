#!/bin/sh -e

echo "Starting rack '$NAME'"

export RACK_ENV=$1

echo $$ > $PIDFILE
if [ -f config.ru ]; then
  exec rackup config.ru -p $PORT
fi

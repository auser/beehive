#!/bin/sh -e

echo "Starting rack '$NAME'"

echo $$ > $PIDFILE
if [ -f config.ru ]; then
  exec rackup config.ru -p $PORT
fi
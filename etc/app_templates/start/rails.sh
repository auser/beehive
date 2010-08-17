#!/bin/sh -e

echo "Starting rails '$NAME'"

echo $$ > $PIDFILE
if [ -f config.ru ]; then
  exec ./script/server -p $PORT
fi
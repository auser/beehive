#!/bin/sh -e

echo "Starting $NAME:$PORT"

echo $$ > $PIDFILE

echo "Starting in directory `pwd` in $1 env"
if [ -f start.sh ]; then
  echo "Using start.sh file"
  exec /bin/sh start.sh
elif [ -f config.ru ]; then
  export RACK_ENV=$1
  exec thin -R config.ru -p $PORT start
fi

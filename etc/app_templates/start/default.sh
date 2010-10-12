#!/bin/sh -e

echo "Starting $NAME:$PORT"

echo $$ > $PIDFILE

echo "Starting in directory `pwd` in $DEPLOY_ENV env"
if [ -f start.sh ]; then
  echo "Using start.sh file"
  exec /bin/sh start.sh
elif [ -f config.ru ]; then
  echo "Using thin with config.ru"
  export RACK_ENV=$DEPLOY_ENV
  exec thin -R config.ru -p $PORT start
fi

#!/bin/sh -e

echo "Starting $NAME:$PORT"

echo $$ > $PIDFILE

echo "Starting in directory `pwd` in $DEPLOY_ENV env"
if [ -f beehive_start.sh ]; then
  echo "Using beehive_start.sh file"
  exec /bin/sh beehive_start.sh
elif [ -f beehive/start.sh ]; then
  echo "Using beehive/start.sh file"
  exec /bin/sh beehive/start.sh
elif [ -f config.ru ]; then
  echo "Using thin with config.ru"
  export RACK_ENV=$DEPLOY_ENV
  exec thin -R config.ru -p $PORT start
fi

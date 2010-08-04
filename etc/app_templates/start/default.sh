#!/bin/sh -e

echo "Starting $NAME -> $PIDFILE"

echo $$ > $PIDFILE
exec /bin/sh start.sh
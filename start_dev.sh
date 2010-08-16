#!/bin/sh
#cd `dirname $0`

# Compile
make compile

# Make root directory
if [ -z "$BEEHIVE_HOME" ]; then
  mkdir -p /tmp/beehive
else
  echo "mkdir -p $BEEHIVE_HOME"
fi

# Start Beehive
echo "Starting beehive"
eval "erl \
    -name beehive@127.0.0.1 \
    -pa deps/*/ebin -pa lib/erlang/apps/*/ebin -pa lib/erlang/apps/*/include  \
    -beehive database_dir '\"$BEEHIVE_HOME/db\"' \
    -eval \"application:start(sasl)\" \
    -eval \"application:start(os_mon)\" \
    -eval \"application:start(crypto)\" \
		-s reloader \
    -eval \"application:start(beehive)\""
#!/usr/bin/env sh
#cd `dirname $0`

# Compile
make compile

# Make rood directory
mkdir -p /tmp/beehive

if [ -z $BEEHIVE_HOME ]; then
  BEEHIVE_HOME="/tmp/beehive"
else
  BEEHIVE_HOME="$BEEHIVE_HOME"
fi

# Start Beehive
echo "Starting beehive"
exec "erl \
    -name beehive@127.0.0.1 \
    -pa deps/*/ebin -pa lib/erlang/apps/*/ebin -pa lib/erlang/apps/*/include  \
    -beehive database_dir '$BEEHIVE_HOME/db' \
    -eval \"application:start(sasl)\" \
    -eval \"application:start(os_mon)\" \
    -eval \"application:start(crypto)\" \
		-s reloader \
    -eval \"application:start(beehive)\""
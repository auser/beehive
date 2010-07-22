#!/usr/bin/env sh
#cd `dirname $0`

# Compile
make compile

# Make rood directory
if [ -z $BEEHIVE_HOME ]; then
  BEEHIVE_HOME="/tmp/beehive"
else
  BEEHIVE_HOME="$BEEHIVE_HOME"
fi

mkdir -p $BEEHIVE_HOME

# Start Beehive
echo "Starting beehive"
erl \
    -name beehive@127.0.0.1 \
    -pa deps/*/ebin -pa lib/erlang/apps/*/ebin -pa lib/erlang/apps/*/include  \
    -beehive database_dir '"$BEEHIVE_HOME/db"' \
    -eval "application:start(sasl)" \
    -eval "application:start(os_mon)" \
    -eval "application:start(crypto)" \
		-s reloader \
    -eval "application:start(beehive)"

#!/bin/sh
#cd `dirname $0`

# Compile
make compile

# We'll start a dummy gem server, if `gem` is present
GEM_BIN=`which gem`

if [ ! -z "$GEM_BIN" ]; then
  if [ -z `ps aux | grep gem | grep server` ]; then
    gem server 2>&1 > /dev/null &
  fi
fi

# Make root directory
if [ -z "$BEEHIVE_HOME" ]; then
  export BEEHIVE_HOME=/tmp/beehive
fi

if [ -z "$BEEHIVE_DOMAIN" ]; then
  export BEEHIVE_DOMAIN=`hostname -f`
fi

mkdir -p $BEEHIVE_HOME

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
#!/usr/bin/env sh
cd `dirname $0`

# Compile
make compile

# Make rood directory
mkdir -p /tmp/beehive

# Start Beehive
echo "Starting beehive"
erl \
    -name beehive@127.0.0.1 \
    -pa deps/*/ebin -pa lib/erlang/apps/*/ebin -pa lib/erlang/apps/*/include  \
    -beehive database_dir '"/tmp/beehive/db"' \
    -eval "application:start(sasl)" \
    -eval "application:start(os_mon)" \
    -eval "application:start(crypto)" \
    -eval "application:start(mnesia)" \
    -eval "application:start(beehive)"

#!/bin/sh

progdir=$(dirname $0)
progname=$(basename $0)
grep=$(which grep)

ERL_OPTS="-pa $PWD/ebin -pa $PWD/include -pz $PWD/deps/*/ebin"
HOSTNAME=`hostname -f`
NAME=control@$HOSTNAME

. `dirname $0`/beehive_env.sh

exec erl \
    -pa "${BEEHIVE_HOME}/ebin" \
    -noinput \
    -hidden \
    $ERL_OPTS \
    -name $NAME \
    -s beehive_control \
    -extra "$@"
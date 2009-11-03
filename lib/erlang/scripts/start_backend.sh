#!/bin/sh -x

cd `dirname ../$0`
DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
if [[ ! -f ebin/router.boot ]]; then
	make boot
fi

HOSTNAME=`hostname`
ERL_NAME=$1
shift

erl -pa $PWD/ebin \
    -pa $PWD/deps/*/ebin \
    -name $ERL_NAME@$HOSTNAME \
    -s reloader \
    -boot backend-0.1 $*

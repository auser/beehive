#!/bin/sh -x

cd `dirname ../$0`
DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
if [[ ! -f ebin/router.boot ]]; then
	make boot
fi

HOSTNAME=`hostname`

erl -pa $PWD/ebin \
    -pz $PWD/deps/*/ebin \
    -name "router@$HOSTNAME" \
    -s reloader \
    -boot router-0.1 $*

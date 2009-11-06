#!/bin/sh -x

cd `dirname ../$0`
DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
if [[ ! -f ebin/router.boot ]]; then
	make boot
fi

HOSTNAME=`hostname`
MNESIA_DIR=$1

# Default to the ./db directory
if [ -z $MNESIA_DIR ]; then
	MNESIA_DIR='"./db"'
fi

erl -pa $PWD/ebin \
    -pz $PWD/deps/*/ebin \
    -name "router@$HOSTNAME" \
    -s reloader \
		-mnesia dir $MNESIA_DIR \
    -boot router-0.1 $*

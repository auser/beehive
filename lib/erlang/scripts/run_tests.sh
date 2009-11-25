#!/bin/sh -x

cd `dirname ../$0`
DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
make

if [[ ! -f ebin/router.boot ]]; then
	make boot
fi

HOSTNAME=`hostname`
MNESIA_DIR=$1

# Default to the ./db directory
if [ -z $MNESIA_DIR ]; then
	MNESIA_DIR='"./test_db"'
fi

erl -pa $PWD/ebin \
    -pz $PWD/deps/*/ebin \
    -name "router@$HOSTNAME" \
    -s reloader \
		-mnesia dir $MNESIA_DIR \
		-s app run_tests \
		-s bee run_tests \
		-s init stop

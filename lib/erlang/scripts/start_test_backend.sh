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
    -name router$ERL_NAME@$HOSTNAME \
    -s reloader \
    -backend app_dir "'./test/fixtures/apps'" script_dir "'./test/fixtures/scripts'" \
    -boot backend-0.1 $*
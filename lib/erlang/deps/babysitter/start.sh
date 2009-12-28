#!/bin/sh

VERSION=$(cat VERSION | tr -d '\n')
PWD=$(dirname $0)
CONFIG=$1

make

if [[ ! -f ebin/babysitter*.boot ]]; then
	make boot
fi

erl -pa $PWD/ebin \
    -s reloader \
    -babysitter \
    -boot babysitter-$VERSION
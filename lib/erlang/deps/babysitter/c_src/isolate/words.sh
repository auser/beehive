#!/bin/sh

cat "$@" | cpp -fpreprocessed | sed 's/[_a-zA-Z0-9][_a-zA-Z0-9]*/x/g' | tr -d ' \012' | wc -c


#!/bin/sh

if [ ! -d test/fixtures/incredibly_simple_rack_app ]; then
  cd ./test/fixtures
  git clone git://github.com/auser/incredibly_simple_rack_app.git
fi
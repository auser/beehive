PACKAGE_NAME = beehive
PACKAGE_VERSION = 0.1

.PHONY: deps compile rel test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

check:
	@echo "Dependencies"
	@./rebar check-deps

clean:
	@./rebar clean

rel: all
	@(cp -Rf ./etc ./rel/files/etc)
	@(make rel_erlang)
	# @(mkdir -p ./rel/beehive/etc)
	# @(cp -Rf etc/* ./rel/beehive/etc)
	# @(cp -Rf bin/* ./rel/beehive/bin)
	# @(chmod u+x ./rel/beehive/bin/*)

rel_erlang:
	@./rebar generate force=1

package:
	@(mkdir -p ./builds)
	@(tar -C rel -c beehive | gzip > ./builds/${PACKAGE_NAME}-${PACKAGE_VERSION}.tar.gz)

test: deps compile
	@./test/bootstrap.sh
	@./rebar skip_deps=true eunit

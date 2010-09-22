PACKAGE_NAME = babysitter
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
	@(cp -Rf etc/app_templates rel/overlay/etc)
	@(cp -f etc/beehive_bee_object_config.conf rel/overlay/etc)
	@(make rel_erlang)
	@(cp -Rf bin/* ./rel/beehive/bin)
	@(chmod u+x ./rel/beehive/bin/start_beehive)
	@(chmod u+x ./rel/beehive/bin/stop_beehive)

rel_erlang:
	@./rebar generate force=1

test: deps compile
	@./test/bootstrap.sh
	@./rebar skip_deps=true eunit suite=$(suite)

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
	@(make rel_erlang)
	@(chmod u+x ./rel/beehive/bin/beehive)

rel_erlang:
	@./rebar generate force=1

test: compile
	./rebar skip_deps=true eunit

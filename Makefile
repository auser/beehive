PACKAGE_NAME = babysitter
PACKAGE_VERSION = 0.1

.PHONY: deps compile rel

all: deps compile

compile:
	@./rebar compile
	
deps:
	@./rebar get-deps
	
clean:
	@./rebar clean
	
rel: all
	@(cp -Rf etc/app_templates rel/overlay/etc)
	@(make rel_erlang)
	@(chmod u+x ./rel/beehive/bin/beehive)

rel_erlang:
	@./rebar generate force=1
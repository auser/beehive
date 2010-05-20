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
	@(make rel_erlang)

rel_erlang:
	@./rebar generate force=1
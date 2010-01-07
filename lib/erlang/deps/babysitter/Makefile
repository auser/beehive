LIBDIR					= $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION					= $(shell cat VERSION | tr -d '\n')
ERL							= erl
EBIN						= ebin
INCLUDE_DIRS 		= include
EBIN_DIRS				= -pa $(EBIN) -pz $(wildcard deps/*/ebin) -pz $(wildcard include/*/ebin)
CFLAGS					= +debug_info -W0 -I $(INCLUDE_DIRS) $(EBIN_DIRS) -noinput +B
ECOMPILE        = $(ERL) $(CFLAGS)
TEST_DIR				= test
TEST_EBIN_DIR		= $(TEST_DIR)/ebin
APP							= babysitter
SUBDIRS         = deps/*
TARGET_DIR			= $(PREFIX)/$(LIBDIR)/$(APP)-$(VERSION)

all: compile

boot: deps
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts babysitter $(VERSION);)

compile: $(TEST_EBIN_DIR)
	@$(ECOMPILE) -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

deps: isolate erlexec

isolate:
	(cd deps/isolate && $(MAKE))

erlexec:
	(cd deps/erlexec && $(MAKE))	

test: compile
	$(ECOMPILE) 	-noshell \
					-pa $(TEST_EBIN_DIR) \
					-s test_suite test \
					-s init stop

$(TEST_EBIN_DIR):
	@mkdir -p $(TEST_EBIN_DIR)

clean:
	echo $(TEST_EBIN_DIR)
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(TEST_EBIN_DIR)/*.beam $(EBIN)/$(APP)-*.tar.gz *.log deps/*/ebin/*.beam deps/erlexec/priv/bin/exec-port deps/isolate/isolate

clean_deps:
	@(cd deps/isolate; $(MAKE) clean)
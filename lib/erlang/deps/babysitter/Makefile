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

ERL_INTERFACE   = $(shell erl -eval 'io:format("~s~n", [code:lib_dir(erl_interface)])' -s init stop -noshell)
CCFLAGS        += -DHAVE_CONFIG_H -I. -Ic_src/ -O -Wall -I$(ERL_INTERFACE)/include
CLDFLAGS       += -L$(ERL_INTERFACE)/lib -lei -lerl_interface -lcap
CC              = g++
EXECUTABLE      = priv/bin/exec-port
CCOMPILE        = $(CC) $(CCFLAGS)
EXEC_ROOT       = c_src/exec

all: compile

boot: deps
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts babysitter $(VERSION);)

compile: $(TEST_EBIN_DIR)
	@$(ECOMPILE) -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

deps: isolate erlexec

isolate:
	(cd c_src/isolate && $(MAKE))
	@(cp c_src/isolate/isolate priv/bin/)

erlexec: exec.o ei++.o
	$(CC) -Wall -g -I/usr/lib/erlang/lib/erl_interface-3.6.2/include -g -O2 -o $(EXECUTABLE) $(EXEC_ROOT)/exec.o $(EXEC_ROOT)/ei++.o  -L/usr/lib/erlang/lib/erl_interface-3.6.2/lib -lei -lcap
	$(CCOMPILE) -g -O2 -o $(EXECUTABLE) $(EXEC_ROOT)/exec.o $(EXEC_ROOT)/ei++.o $(CLDFLAGS)
	@(chmod +x $(EXECUTABLE))
	@(rm $(EXEC_ROOT)/*.o)
	
ei++.o:
	$(CCOMPILE) -o $(EXEC_ROOT)/ei++.o -c $(EXEC_ROOT)/ei++.cpp

exec.o:
	$(CCOMPILE) -o $(EXEC_ROOT)/exec.o -c $(EXEC_ROOT)/exec.cpp


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
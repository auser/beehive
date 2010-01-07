LIBDIR					= $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION					= $(shell cat VERSION | tr -d '\n')
ERL							= erl
EBIN						= ebin
INCLUDE_DIRS 		= include
EBIN_DIRS				= -pa $(EBIN) -pz $(wildcard deps/*/ebin) -pz $(wildcard include/*/ebin)
CFLAGS					= +debug_info -W0 -I $(INCLUDE_DIRS) $(EBIN_DIRS) -noinput +B
ECOMPILE        = $(ERL) $(CFLAGS)
APP							= erlexec
ERL_INTERFACE   = $(shell erl -eval 'io:format("~s~n", [code:lib_dir(erl_interface)])' -s init stop -noshell)
CCFLAGS        += -DHAVE_CONFIG_H -I. -Ic_src/ -O -Wall -I$(ERL_INTERFACE)/include
CLDFLAGS       += -L$(ERL_INTERFACE)/lib -lei -lerl_interface -lcap
CC              = g++
EXECUTABLE      = priv/bin/exec-port
CCOMPILE        = $(CC) $(CCFLAGS)

all: compile

compile: c_src
	@$(ECOMPILE) -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

c_src: exec.o ei++.o
	g++ -Wall -g -I/usr/lib/erlang/lib/erl_interface-3.6.2/include -g -O2 -o exec-port exec.o ei++.o  -L/usr/lib/erlang/lib/erl_interface-3.6.2/lib -lei -lcap  
	$(CCOMPILE) -g -O2 -o $(EXECUTABLE) exec.o ei++.o $(CLDFLAGS)
	@(chmod +x $(EXECUTABLE))
	@(rm *.o)
	
ei++.o:
	$(CCOMPILE) -o ei++.o -c c_src/ei++.cpp

exec.o:
	$(CCOMPILE) -o exec.o -c c_src/exec.cpp

clean:
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(TEST_EBIN_DIR)/*.beam $(EBIN)/$(APP)-*.tar.gz *.log 

clean_deps:
	@(cd deps/isolate; $(MAKE) clean)
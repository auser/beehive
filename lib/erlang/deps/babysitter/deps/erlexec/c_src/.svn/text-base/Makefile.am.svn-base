
ERL_INTERFACE = @ERLANG_LIB_DIR_erl_interface@
ARCH = @ARCH@

AM_CXXFLAGS = -Wall -g -I$(ERL_INTERFACE)/include

privbindir = $(ERLANG_INSTALL_LIB_DIR_exec)/priv/$(ARCH)/bin

privbin_PROGRAMS = exec-port
exec_port_SOURCES = exec.cpp ei++.cpp
exec_port_LDADD = -L$(ERL_INTERFACE)/lib -lei -lcap

.INTERMEDIATE: exec.o ei++.o

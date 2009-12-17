LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= $(shell cat VERSION | tr -d '\n')
CC							= erlc
ERL							= erl
EBIN						= ebin
INCLUDE_DIRS 		= include
CFLAGS					= +debug_info -W0 -I $(INCLUDE_DIRS) -pa $(EBIN) -I gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
DEPS_DIR 				= deps
EBIN_DIRS				= $(wildcard $(DEPS_DIR)/*/ebin) $(wildcard include/*/ebin)
TEST_DIR				= test
TEST_EBIN_DIR		= $(TEST_DIR)/ebin
APP							= beehive

RELFILE = $(EBIN)/$(APP)-$(VERSION).rel

all: compile
  
deps: mochi
	
mochi:
	@(cd deps/mochiweb;$(MAKE))

boot:
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts beehive $(VERSION);)
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts router $(VERSION);)
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts storage $(VERSION);)
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts node $(VERSION);)

compile: $(TEST_EBIN_DIR)
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'
	
debug: $(TEST_EBIN_DIR)
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all([{d, debug}]) of up_to_date -> halt(0); error -> halt(1) end.'
	
test: compile
	$(ERL) 	-noshell -pa $(EBIN) \
					-pa deps/*/ebin \
					-pa $(TEST_EBIN_DIR) \
					-s test_suite test \
					-s init stop
	
edoc:
	@echo Generating $(APP) documentation from srcs
	@$(ERL) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt

$(TEST_EBIN_DIR):
	@mkdir -p $(TEST_EBIN_DIR)

clean:
	echo $(TEST_EBIN_DIR)
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(TEST_EBIN_DIR)/*.beam $(EBIN)/$(APP)-*.tar.gz *.log

clean_deps:
	@(cd deps/mochiweb; $(MAKE) clean)
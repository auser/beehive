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

all: $(TEST_EBIN_DIR) ebin compile
all_boot: all boot
wonderland_boot: wonderland all_boot
start: all start_all
rstakeout: wonderland compile

deps: mochi cp_dep_beams

mochi:
	(cd deps/mochiweb;$(MAKE))
gen_cluster:
	(cd deps/gen_cluster;$(MAKE))
# rabbitmq_server:
# 	(cd deps/rabbitmq-server;$(MAKE))

cp_dep_beams:
	cp $(DEPS_DIR)/*/ebin/*.beam $(EBIN)

compile: deps
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@echo Generating $(APP) documentation from srcs
	@$(ERL) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt

eunit:
	cd test/include/eunit && make

shell: compile
	$(ERL) 	-sname shell -setcookie test \
					-pa $(EBIN) \
					-pa deps/*/ebin

test: $(TEST_EBIN_DIR) compile
	$(ERL) 	-noshell -pa $(EBIN) \
					-pa deps/*/ebin \
					-pa $(TEST_EBIN_DIR) \
					-pa test/include/gen_server_mock/ebin \
					-s test_suite test \
					-s init stop
	
boot: compile
	(cd $(EBIN); erl -pa ../$(EBIN) $(DEP_EBIN_DIRS_DOTDOT) -noshell -run make_boot write_scripts $(APP) $(VERSION))

release:
	(cd $(EBIN); erl -pa ../$(EBIN) $(DEP_EBIN_DIRS_DOTDOT) -noshell -run make_boot write_release_scripts $(APP) $(VERSION))

target_system: $(RELFILE)
	escript scripts/target_system create "ebin/$(APP)-$(VERSION)"

inspect_target_system:
	exec tar tf ebin/$(APP)-$(VERSION).tar.gz

start_all:
	(cd $(EBIN); erl -pa $(EBIN) -noshell -sname $(APP) -boot $(APP))

$(EBIN):
	@mkdir $(EBIN)

clean:
	echo $(TEST_EBIN_DIR)
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(TEST_EBIN_DIR)/*.beam $(EBIN)/$(APP)-*.tar.gz

clean_mochiweb:
	rm -rf deps/mochiweb/ebin/*.beam

$(TEST_EBIN_DIR):
	@mkdir $(TEST_EBIN_DIR)

$(RELFILE): boot

# tmp, testing for nate
nates_delivery:
	scp scripts/target_system vm:~/$(APP)
	scp ebin/$(APP)-0.0.1.tar.gz vm:~/$(APP) 
	# remotely:
	# rm -rf /usr/local/$(APP) && ./target_system install $(APP)-0.0.1 /usr/local/$(APP)
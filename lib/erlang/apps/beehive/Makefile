all:
	echo "hello_world"
	./rebar compile

test: all
	@mkdir -p tests_ebin
	@cd tests;erl -make
	@erl -noshell -boot start_sasl -pa ebin -pa tests_ebin -s luke -eval 'test_suite:test().' -s init stop
	@rm -f ebin/test_* ebin/*_tests.erl

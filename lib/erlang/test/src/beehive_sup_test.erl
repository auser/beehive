-module (beehive_sup_test).
-include ("beehive.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.

teardown(_S) ->
  ok.

test_info_msg_test_() ->
  {
    setup, 
    fun setup/0, 
    fun teardown/1,
    [
      test_compile_applications()
    ]
  }.

%%====================================================================
%% TESTS
%%====================================================================
test_compile_applications() ->
  YesAppDef       = { yes_module,   {yes_module, start_link, []}, permanent,2000,worker,[]},
  NoAppDef        = { no_module,    {no_module, start_link, []}, permanent,2000,worker,[]},
  RandomeAppDef   = { random_app,    {random_app, start_link, []}, permanent,2000,worker,[]},
  
  % application:set_env(beehive, no, true),
  % 
  % Apps1 = beehive_sup:compile_applications([{yes, YesAppDef}, {no, NoAppDef}, {random, RandomeAppDef}]),
  % ?assertEqual([YesAppDef, RandomeAppDef], Apps1),
  % 
  % % % Now test that they DO get set
  % application:set_env(beehive, no, false),
  % Apps2 = beehive_sup:compile_applications([{yes, YesAppDef}, {no, NoAppDef}, {random, RandomeAppDef}]),
  % ?assertEqual([NoAppDef, YesAppDef, RandomeAppDef], Apps2),
  
  application:set_env(beehive, no, true),
  application:set_env(beehive, random, true),
  application:set_env(beehive, yes, undefined),
  
  Apps3 = beehive_sup:compile_applications([{yes, YesAppDef}, {no, NoAppDef}, {random, RandomeAppDef}]),
  ?assertEqual([YesAppDef], Apps3).
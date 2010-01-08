%%%-------------------------------------------------------------------
%%% File    : app_utils.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jan  8 14:57:14 PST 2010
%%%-------------------------------------------------------------------

-module (app_utils).

-export ([
  type_template/2
]).

% Type template
% Get the application type of the template
type_template(Type, Proplist) ->
  ?USER_OR_BH(Type)
  ok.
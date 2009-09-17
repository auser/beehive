-module (app_registry_client).
-include ("beehive.hrl").
-export ([start_and_link/2]).

start_and_link(AppName, Port) ->
  _RegistryNode = case global:whereis_name(app_registry_srv) of
    undefined -> app_registry_app:start(normal, []);
    P -> P
  end,
  Args = [
    {name, AppName}, {host, get_my_hostname()}, {port, Port}
  ],
  app_registry_srv:register_application(Args).
  
get_my_hostname() ->
  {ok, Interfaces} = inet:getif(),
  [Interface|_] = Interfaces,
  element(1, Interface).
  
-include_lib("kernel/include/inet.hrl"). % needed for hostent structure
% get_client_name(Socket) ->
%   case inet:peername(Socket) of
%   {error, _Reason} -> "?.?.?.?:?";
%   {ok, {{A,B,C,D}, Port}} ->
%     case inet:gethostbyaddr({A,B,C,D}) of
%     {error, _Reason} ->
%       io_lib:fwrite("~B.~B.~B.~B:~B", [A,B,C,D, Port]);
%     {ok, Hostent} ->
%       io_lib:fwrite("~s:~B", [Hostent#hostent.h_name, Port])
%     end
%   end.

%%%-------------------------------------------------------------------
%%% File    : users_controller.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sat Nov 28 23:03:51 PST 2009
%%%-------------------------------------------------------------------

-module (users_controller).

-include ("beehive.hrl").
-include ("http.hrl").
-export ([get/2, post/2, put/2, delete/2]).

get([Email, "apps"], _Data) ->
  case users:find_by_email(Email) of 
		[] -> {Email, "does_not_exist"}; 
		User -> 
		Apps = user_apps:all_apps(User#user.email),
		{
			user, [{"email", User#user.email}, {"level", User#user.level},
			{apps, lists:map(fun(App) -> {name, App#app.name} end, Apps)}]
		}
	end;
get([Email], _Data) ->
	case users:find_by_email(Email) of 
		[] -> {Email, "does_not_exist"}; 
		User -> {
			"user", [{"email", User#user.email}, {"level", User#user.level}]
		}
	end;
get(_, _Data) -> 
  All = users:all(),
  { "users", lists:map(fun(A) ->
      [{"level", A#user.level}, {"email", A#user.email}]
    end, All)
  }.

post([Name, "keys", "new"], Data) ->
  auth_utils:run_if_admin(fun(_) ->
    case proplists:get_value(key, Data) of
      undefined -> error("No key defined");
      Key ->
        case users:find_by_email(Name) of
          User when is_record(User, user) ->
            case users:create(User#user{key = Key}) of
              User when is_record(User, user) -> 
                [{"user", User#user.email}, {"key", "added key"}];
              _Else -> 
                error("There was an error adding bee")
            end;
          _E ->
            error("Error finding user")
        end
      end
    end, Data);
  
post(["new"], Data) ->
  auth_utils:run_if_admin(fun(_) ->
    
    case proplists:get_value(email, Data) of
      undefined -> {error, "No email defined"};
      Email ->
        % The user has been submitted with an email
        case users:exist(Email) of
          true -> {error, "The user already exists"};
          false ->
            case users:create(Data) of
              User when is_record(User, user) -> 
                {user, [{email, User#user.email}]};
              E -> 
                io:format("Error: ~p~n", [E]),
                {error, "There was an error adding bee"}
            end
        end
    end
    
  end, Data);
      
post(Path, _Data) -> 
  io:format("Path: ~p~n", [Path]),
  error("unhandled").
put(_Path, _Data) -> "unhandled".

delete([], Data) ->
  auth_utils:run_if_admin(fun(_) ->
    case proplists:is_defined(email, Data) of
      false -> misc_utils:to_bin("No email given");
      true ->
        Email = proplists:get_value(email, Data),
        users:delete(Email)
    end
  end, Data);
delete(_Path, _Data) -> "unhandled".

error(Msg) ->
  {struct, [{error, misc_utils:to_bin(Msg)}]}.
%%%-------------------------------------------------------------------
%%% File    : bh_upload_pack.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%   Based on Tom Preston-Werner's egitd (http://github.com/mojombo/egitd)
%%% Created :  Wed Dec  2 20:07:01 PST 2009
%%%-------------------------------------------------------------------

-module (bh_upload_pack).

-export([handle/3]).

-define(READ_SOCKET_TIMEOUT, 10).
-define(READ_PORT_TIMEOUT, 100).

%****************************************************************************
%
% Entry
%
%****************************************************************************

handle(Sock, Host, Header) ->
  extract_repo_path(Sock, Host, Header).

%****************************************************************************
%
% Main flow
%
%****************************************************************************

% Extract the repo from the header.
extract_repo_path(Sock, Host, Header) ->
  io:format("extract_repo_path Header: ~s~n", [Header]),
  case re:run(Header, " /(.*.git)") of
    {match, Matches} ->
      {Start, Length} = hd(lists:reverse(Matches)),
      Path = string:substr(Header, Start + 1, Length),
      io:format("Path: ~s~n", [Path]),
      convert_path(Sock, Host, Path);
    _Else ->
      invalid
  end.

% Convert the repo path to an absolute path as specified by the config file.
convert_path(Sock, Host, Path) ->
  case conf:convert_path(Host, Path) of
    {ok, FullPath} ->
      repo_existence(Sock, Host, Path, FullPath);
    {error, nomatch} ->
       error_logger:info_msg("no repo match: ~p~n", [Path]),
       gen_tcp:send(Sock, "003b\n*********'\n\nNo matching repositories found.\n\n*********"),
       ok = gen_tcp:close(Sock)
  end.

% Ensure that the repo exists.
repo_existence(Sock, Host, Path, FullPath) ->
  case file_exists(FullPath) of
    true ->
      export_ok(Sock, Host, Path, FullPath);
    false ->
      repo_existence_ext(Sock, Host, Path, FullPath)
  end.
  
% The repo may always be specified without .git on the end
repo_existence_ext(Sock, Host, Path, FullPath) ->
  FullPathExt = FullPath ++ ".git",
  case file_exists(FullPathExt) of
    true ->
      export_ok(Sock, Host, Path, FullPathExt);
    false ->
      error_logger:info_msg("no such repo: ~p~n", [FullPath]),
      gen_tcp:send(Sock, "003b\n*********'\n\nNo matching repositories found.\n\n*********"),
      ok = gen_tcp:close(Sock)
  end.

% Ensure that a 'git-daemon-export-ok' file is present in the repo.
export_ok(Sock, Host, Path, FullPath) ->
  GitDaemonExportOkFilePath = filename:join([FullPath, "git-daemon-export-ok"]),
  case file_exists(GitDaemonExportOkFilePath) of
    true ->
      make_port(Sock, Host, Path, FullPath);
    false ->
      error_logger:info_msg("permission denied to repo: ~p~n", [FullPath]),
      gen_tcp:send(Sock, "0048\n*********'\n\nPermission denied. Repository is not public.\n\n*********"),
      ok = gen_tcp:close(Sock)
  end.

% Create the port to 'git upload-pack'.
make_port(Sock, _Host, _Path, FullPath) ->
  Command = "git upload-pack " ++ FullPath,
  Port = open_port({spawn, Command}, [binary]),
  send_port_to_socket(Port, Sock).

% Send output from port to socket
send_port_to_socket(Port, Sock) ->
  receive
    {Port, {data, Data}} ->
      % io:format("port(~p) = ~p~n", [erlang:size(Data), Data]),
      gen_tcp:send(Sock, Data),
      case erlang:size(Data) of
        16384 ->
          send_port_to_socket(Port, Sock);
        _SizeElse ->
          case last_byte(Data) of
            10 ->
             send_port_to_socket(Port, Sock);
            13 ->
              send_port_to_socket(Port, Sock);
            _ByteElse ->
              send_socket_to_port(Port, Sock)
          end
      end;
    Msg ->
      error_logger:error_msg("unknown message ~p~n", [Msg]),
      send_socket_to_port(Port, Sock)
    after ?READ_PORT_TIMEOUT ->
      error_logger:error_msg("timed out waiting for port~n"),
      send_socket_to_port(Port, Sock)
  end.

% Send input from socket to port
send_socket_to_port(Port, Sock) ->
  case gen_tcp:recv(Sock, 0, ?READ_SOCKET_TIMEOUT) of
    {ok, Data} ->
      % io:format("socket = ~p~n", [Data]),
      port_command(Port, Data),
      send_port_to_socket(Port, Sock);
    {error, timeout} ->
      error_logger:error_msg("read socket timeout~n", []),
      send_port_to_socket(Port, Sock);
    {error, Reason} ->
      error_logger:error_msg("read socket error ~p~n", [Reason])
  end.

file_exists(FullPath) ->
  case file:read_file_info(FullPath) of
    {ok, _Info}      -> true;
    {error, _Reason} -> false
  end.

last_byte(Bin) ->
  Size = erlang:size(Bin),
  {_B1, B2} = split_binary(Bin, Size - 1),
  [Byte] = binary_to_list(B2),
  Byte.
-module (rabbit_router).

-export ([get_path_for/2]).

get_path_for(Path, Req) ->
  Path.
-module (home_controller).
-export ([get/1, post/2, put/2, delete/2]).

get(_) -> "hello world".

post(_Path, _Data) -> "unhandled".
put(_Path, _Data) -> "unhandled".
delete(_Path, _Data) -> "unhandled".
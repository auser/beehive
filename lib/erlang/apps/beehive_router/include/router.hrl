-define (IDLE_TIMEOUT, 40000).
-define (MAX_HEADERS, 100).
-define (CONNECT_TIMEOUT, timer:seconds(5)).

-include ("beehive.hrl").
-include ("common.hrl").
-include ("http.hrl").
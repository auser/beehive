Router
===

How it works
===
There are 2 mnesia tables setup by the beehive router:

<img src="http://i36.tinypic.com/t023xw.png" alt="table" />

<pre><code>
  |----------|  |-------------|
  | backends |  | apps        |
  |----------|  |-------------|
  | app_name |  | name        |
  | host     |  | pwd         |
  | ...      |  | ...         |
  |----------|  |-------------|               
</code></pre>

## Proxy
backend_srv:add_backend({"streaming",{127,0,0,1}, 5001}).
backend_srv:add_backend({"srdves3", "services.speak4it.com", 80}).
backend_srv:get_state().


event_manager:notify({info, "hi"}).

## Add an application

## Via REST:

  curl -i -XPOST -d"{\"name\":\"beehive.com\"}" beehive.com:8080/new
  curl -i -XPOST -d"{\"name\":\"applebees.com\"}" beehive.com:8080/new

Viewing the list of supported apps:

  curl -i beehive.com:8080/status

DEVELOPER DATA
===

Each request goes through the following process:
<img src="http://www.websequencediagrams.com/cgi-bin/cdraw?lz=Q2xpZW50LT5Tb2NrZXRTZXJ2ZXI6IEluaXRpYWwgcmVxdWVzdAoAEgwtPlByb3h5SGFuZGwAIgp0ZSAACwwKABgMLT5SAEIGRGVjb2RlcjogAAMGAFYJABEOLT5BcHBTcnY6IENob3NlIGFuIGF2YWlsYWJsZSBiYWNrZW5kCgAdBgB8EEVuZ2FnZSBhbmQgY29ubmVjdCB0bwArCQB2HlNlbmQgaQCBaw5cbiB0aHJvdWdoIHRvIACCGAYAgR0RAII9BjogU3Bhd24gbGlzdGVuZXIgZm9yIGMAglkFIGRhdGEAgVQRAIJkCAAkE3MAgwIFADIF&s=rose" />


TODO
===
  * Add ets/mnesia storage to the front-end servers
  * Add more than just http servers (abstract the proxying protocols)
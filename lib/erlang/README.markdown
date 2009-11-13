Router
===

How it works
===
There are 2 mnesia tables setup by the beehive router:

<img src="http://i36.tinypic.com/t023xw.png" alt="table" />

The app table stores the applications associated with Beehive.

The backend table stores the backend data, and their state.

## Proxy

The proxy can be hot-loaded with new routes simply with a RESTful interface. The name (the routing key), the endpoint host and the port of the backend need to be included. For instance:

<pre><code>
  curl -i -XPOST -d"{\"app_name\":\"test\", \"host\":\"google.com\", \"port\":\"80\"}" beehive.com:8080/backend/new
</code></pre>

The parameters that must be included are the app_name (the routing key), the host and the port. You can check to make sure that they were added by visiting the page: http://beehive.com:8080/backend/all

These can also be added at the erlang command-line by:

<pre><code>
  backend_srv:add_backend({"streaming",{127,0,0,1}, 5001}).
  backend_srv:add_backend({"search", "google.com", 80}).
</code></pre>

## Apps
Adding an application can also be added via the RESTful interface. For example:

<pre><code>
  curl -i -XPOST -d"{\"name\":\"beehive.com\"}" beehive.com:8080/apps/new
  curl -i -XPOST -d"{\"name\":\"applebees.com\"}" beehive.com:8080/apps/new
</code></pre>

Viewing the list of supported apps:

  curl -i beehive.com:8080/status

Note: The documentation assumes that the router is sitting at a network accessible location. The documentation uses the CNAME "beehive.com" to illustrate.


DEVELOPER DATA
===

Each request goes through the following process:
<img src="http://www.websequencediagrams.com/cgi-bin/cdraw?lz=Q2xpZW50LT5Tb2NrZXRTZXJ2ZXI6IEluaXRpYWwgcmVxdWVzdAoAEgwtPlByb3h5SGFuZGwAIgp0ZSAACwwKABgMLT5SAEIGRGVjb2RlcjogAAMGAFYJABEOLT5BcHBTcnY6IENob3NlIGFuIGF2YWlsYWJsZSBiYWNrZW5kCgAdBgB8EEVuZ2FnZSBhbmQgY29ubmVjdCB0bwArCQB2HlNlbmQgaQCBaw5cbiB0aHJvdWdoIHRvIACCGAYAgR0RAII9BjogU3Bhd24gbGlzdGVuZXIgZm9yIGMAglkFIGRhdGEAgVQRAIJkCAAkE3MAgwIFADIF&s=rose" />

Glossary
===
  * Backend - A backend service to forward and route requests
  * App - An application with multiple backends

TODO
===
  * Add ets/mnesia storage to the front-end servers
  * Add more than just http servers (abstract the proxying protocols)
Beehive Router
===

Getting started
===

To start the router:
<pre><code>
  make && make boot
  ./scripts/start_beehive.sh
</code></pre>

This will start the basic router with the default options. The default node type that gets started is the router type. You can start a node (see glossary below) with

<pre><code>
  ./scripts/start_beehive.sh -t node
</code></pre>

There are many options to starting the router, for more information and options, type:

<pre><code>
  ./scripts/start_beehive.sh -h
</code></pre>

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
  % or
  backend_srv:add_backend([{app_name, "streaming"}, {host, "127.0.0.1"}, {port, 5001}]).
</code></pre>

## Apps
Adding an application can also be added via the RESTful interface. For example:

<pre><code>
  curl -i -XPOST -d"{\"name\":\"beehive.com\"}" beehive.com:8080/app/new
  curl -i -XPOST -d"{\"name\":\"test\"}" beehive.com:8080/app/new
</code></pre>

Viewing the list of supported apps:

<pre><code>
  curl -i beehive.com:8080/app
</code></pre>

## Nodes
Beehive is a distributed system. You can add multiple nodes in the router. The node_manager handles the node connections.

To start a new

Note: The documentation assumes that the router is sitting at a network accessible location. The documentation uses the CNAME "beehive.com" to illustrate.


DEVELOPER DATA
===

Each request goes through the following process:
<img src="http://www.websequencediagrams.com/cgi-bin/cdraw?lz=Q2xpZW50LT4AAgZSZXF1ZXN0SGFuZGxlcjogSW5pdGlhbCByABIGCgASFC0-UHJveHkAKwlTcGF3biBuZXdcbiAADgwKABsMLT5IVFRQAGgHRGVjb2RlcjogSGFuZCBvdmVyIFxuYwCBFAUAcQkAHRIAaBBSZXR1cm4gcGFyc2VkAIEpCQBnDlNlcnZlclNlbGVjdG9yOiBDaG9vc2UgYW5cbiBhdmFpbGFibGUgYmFja2VuZAoAIA4AgVgQQ29ubmVjdCBhbmQgXG5lbmdhZwAxCgBjFDogU2VuZCBpAIJGDiB0byAAgREGAIISDwCDFAYAgkgIbGlzdGVuAIIVBWZvciAAghcHZGF0YQBUGAAjFHMAgXsFADIF&s=rose" />

Glossary
===
  * Backend - A backend service to forward and route requests
  * App - An application with multiple backends
  * Node - A machine that is capable of hosting applications
  * Routing Node - A router node that is a part of the routing mesh

TODO
===
  * Add ets/mnesia storage to the front-end servers
  * Add backend dets file for recovery when the router dies
  * Add more than just http servers (abstract the proxying protocols)
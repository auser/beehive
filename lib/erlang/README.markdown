Beehive Router
===

Getting started
===

To start the router:
<pre><code>
  make && make boot
  ./scripts/start_beehive.sh
</code></pre>

This will start the basic router with the default options. The default node type that gets started is the router type. You can start a node (see glossary below) with the following command. Node that the -s argument refers to a seed node. This is any node already in the mesh.

<pre><code>
  ./scripts/start_beehive.sh -t node -n bob@mymac.local -s router@mymac.local
</code></pre>

There are many options to starting the router, for more information and options, type:

<pre><code>
  ./scripts/start_beehive.sh -h
</code></pre>

To start with a list of bees, use the -i option to point to a file that looks like:

<pre><code>
  {"app1", "ec2-67-202-21-173.compute-1.amazonaws.com", 8080}.
  {"app2", "ec2-174-129-54-214.compute-1.amazonaws.com", 8080}.
</code></pre>

<pre><code>
  ./scripts/start_beehive.sh -i /path/to/the/file/from/above
</code></pre>

How it works
===
There are 2 mnesia tables setup by the beehive router:

<img src="http://i36.tinypic.com/t023xw.png" alt="table" />

The app table stores the applications associated with Beehive.

The bee table stores the bee data, and their state.

## Users

Beehive has basic support for user accounts. The root user account information is:
<pre><code>username: root@getbeehive.com
  password: 098f6bcd4621d373cade4e832627b4f6
</code></pre>

## Proxy

The proxy can be hot-loaded with new routes simply with a RESTful interface. The name (the routing key), the endpoint host and the port of the bee need to be included. For instance:

<pre><code>
  curl -i -XPOST -d"{\"app_name\":\"test\", \"host\":\"google.com\", \"port\":\"80\"}" beehive.com:8080/bee/new
</code></pre>

The parameters that must be included are the app_name (the routing key), the host and the port. You can check to make sure that they were added by visiting the page: http://beehive.com:8080/bee/all

These can also be added at the erlang command-line by:

<pre><code>
  bee_srv:add_bee({"streaming",{127,0,0,1}, 5001}).
  % or
  bee_srv:add_bee([{app_name, "streaming"}, {host, "127.0.0.1"}, {port, 5001}]).
</code></pre>

## Apps
Adding an application can also be added via the RESTful interface. For example:

<pre><code>
  curl -i -XPOST -d"{\"name\":\"beehive\", \"host\":\"ec2-75-121-34-215-amazon.com\", \"port\":\"8080\"}" beehive.com:8080/app/new
  curl -i -XPOST -d"{\"name\":\"test\", \"host\":\"ec2-75-121-34-210-amazon.com\", \"port\":\"8081\"}" beehive.com:8080/app/new
</code></pre>

Viewing the list of supported apps:

<pre><code>
  curl -i beehive.com:8080/app
</code></pre>

All operations can be handled in a RESTful interface.

For instance, to terminate and restart the application in the beehive, issue a request such as:

<pre><code>
  curl -i -XPOST http://beehive.com:8080/apps/[app_name]/restart
</code></pre>

## Nodes
Beehive is a distributed system. You can add multiple nodes in the router. The node_manager handles the node connections.

Viewing the nodes is as easy as a query as well:

<pre><code>
  curl -i beehive.com:8080/nodes
</code></pre>

To add a new node, as mentioned above, start a node with the seed value from the start script:
<pre><code>
  ./start_beehive.sh -s 'router@my-other-machine.com'
</code></pre>

To add an existing node to a cluster, you can set the seed with:

<pre><code>
  node_manager:set_seed(OtherNodePid).
</code></pre>

Advanced
===
The core functionality of Beehive is event-driven. It supports user defined callbacks as well. To hook into the beehive architecture, you will have to write a custom handler that exports the function: handle_event/1. And example of this custom event handler might look something like this:

<pre><code>
  -module (my_callback_handler).
  -include ("/path/to/include/beehive.hrl").

  -export ([handle_event/1]).

  handle_event({bee, ready, Bee}) ->
    io:format("The bees: ~p is ready~n", [Bee#bee.id]);

  handle_event(_) -> ok.
</code></pre>

Notice that the module name is pretty unique. It's a good idea to stick to a unique name so as not to clash with Beehive modules.

In any case, the handler for the event will catch the {bee, ready, Bee} event thrown when a backend has been released from a single connection. 

All the events in the source are thrown with the method: ?NOTIFY(EventTuple) and are documented throughout the source. Here is a brief list of the callbacks available to handle:

<table><tr><th>Event</th><th>Description</th></tr>
  <tr><td>{bee, used, Bee}</td><td>When a connection has been established</td></tr>
  <tr><td>{bee, ready, Bee}</td><td>When a connection has been terminated</td></tr>
  <tr><td>{bee, cannot_connect, Bee}</td><td>When a connection could not be made to the bee</td></tr>
  <tr><td>{bee, bee_down, Bee}</td><td>Fired when the backend is requested to be terminated</td></tr>
  <tr><td>{app, updated, App}</td><td>Fired when the app has been updated</td></tr>
  <tr><td>{app, request_to_start_new_bee, Name}</td><td>Fired when the app has been queried and there are not enough bees</td></tr>
</table>

Note that this will *not* alter the functionality of beehive, it can only <strong>enhance</strong> usage for the user.

Note: The documentation assumes that the router is sitting at a network accessible location. The documentation uses the CNAME "beehive.com" to illustrate.

To start a router with the custom callback module, use the -c and -a switches:

<pre><code>
  ./scripts/start_beehive.sh -a /path/to/custom_callback_module -c custom_callback_module
</code></pre>

DEVELOPER DATA
===

Each request goes through the following process:
<img src="http://www.websequencediagrams.com/cgi-bin/cdraw?lz=Q2xpZW50LT4AAgZSZXF1ZXN0SGFuZGxlcjogSW5pdGlhbCByABIGCgASFC0-UHJveHkAKwlTcGF3biBuZXdcbiAADgwKABsMLT5IVFRQAGgHRGVjb2RlcjogSGFuZCBvdmVyIFxuYwCBFAUAcQkAHRIAaBBSZXR1cm4gcGFyc2VkAIEpCQBnDlNlcnZlclNlbGVjdG9yOiBDaG9vc2UgYW5cbiBhdmFpbGFibGUgYmFja2VuZAoAIA4AgVgQQ29ubmVjdCBhbmQgXG5lbmdhZwAxCgBjFDogU2VuZCBpAIJGDiB0byAAgREGAIISDwCDFAYAgkgIbGlzdGVuAIIVBWZvciAAghcHZGF0YQBUGAAjFHMAgXsFADIF&s=rose" />

Glossary
===
  * Bee - A bee end point service to forward and route requests
  * App - An application with multiple bees
  * Node - A machine that is capable of hosting applications
  * Routing Node - A router node that is a part of the routing mesh

TODO
===
  * Add ets/mnesia storage to the front-end servers
  * Add bee dets file for recovery when the router dies
  * Add more than just http servers (abstract the proxying protocols)
  * Propagate the custom callback module across the mesh
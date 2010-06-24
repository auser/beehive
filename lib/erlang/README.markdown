Beehive Router
===

Getting started
===

To start the router:

    make
    ./bin/start_beehive

This will start the basic router with the default options. The default node type that gets started is the router type. You can start a node (see glossary below) with the following command. Node that the -s argument refers to a seed node. This is any node already in the mesh.

    ./scripts/start_beehive.sh -t node -n bob@mymac.local -s router@mymac.local

You also need to start some storage backends to store the squashed bees. This is easy to do with the ./scripts/start_beehive.sh script:

    ./scripts/start_beehive.sh -t storage -s router@mymac.local

There are many options to starting the router, for more information and options, type:

    ./scripts/start_beehive.sh -h

To start with a list of bees, use the -i option to point to a file that looks like:

    {"app1", "ec2-67-202-21-173.compute-1.amazonaws.com", 8080}.
    {"app2", "ec2-174-129-54-214.compute-1.amazonaws.com", 8080}.

To start it, run:

    ./scripts/start_beehive.sh -i /path/to/the/file/from/above

How it works
===
There are 4 mnesia tables setup by the beehive router:

<img src="https://github.com/auser/beehive/raw/master/lib/erlang/docs/tables.png" width="300" alt="table" />

The app table stores the applications associated with Beehive.

The bee table stores the bee data, and their state.

The user table stores information about the users associated with the system.

The user_app table stores the mappings between users and their apps

## Configuration

There are multiple methods of configuration. If you are going to configure the router the same way every time, the recommended method is to write a configuration file and pass in the location of the configuration file at the start command with the -c option, like so:

    ./scripts/start_beehive.sh -c "/path/to/configuration/file"
    
The configuration file must be in the following format:
    
    {parameter_name, value}.
    
These are erlang tuples, which is how beehive parses the configuration file. For samples, check out the sample configuration files here: [http://github.com/auser/beehive/tree/master/lib/erlang/config](http://github.com/auser/beehive/tree/master/lib/erlang/config).

All of the available variables that can be overridden can be overridden on the command-line as well. To see all of the available variables, run ./start_beehive.sh with the '-h' switch.

## Proxy

The proxy can be hot-loaded with new routes simply with a RESTful interface. The name (the routing key), the endpoint host and the port of the bee need to be included. For instance:

    curl -i -XPOST -d"{\"app_name\":\"test\", \"host\":\"google.com\", \"port\":\"80\"}" beehive.com:8080/bee/new

The parameters that must be included are the app_name (the routing key), the host and the port. You can check to make sure that they were added by visiting the page: http://beehive.com:8080/bee/all

These can also be added at the erlang command-line by:

    bee_store:add_bee({"streaming",{127,0,0,1}, 5001}).
    % or
    bee_store:add_bee([{app_name, "streaming"}, {host, "127.0.0.1"}, {port, 5001}]).

## Apps
Adding an application can also be added via the RESTful interface. For example:

    curl -i -XPOST -d"{\"name\":\"beehive\", \"host\":\"ec2-75-121-34-215-amazon.com\", \"port\":\"8080\"}" beehive.com:8080/app/new
    curl -i -XPOST -d"{\"name\":\"test\", \"host\":\"ec2-75-121-34-210-amazon.com\", \"port\":\"8081\"}" beehive.com:8080/app/new

Viewing the list of supported apps:

    curl -i beehive.com:8080/app

All operations can be handled in a RESTful interface.

For instance, to terminate and restart the application in the beehive, issue a request such as:

    curl -i -XPOST http://beehive.com:8080/apps/[app_name]/restart

## Storage nodes
To store the distributable bees, you must start a storage backend. Beehive makes this easy again by using the start script:

    ./scripts/start_beehive.sh -t storage

Note, these storage backends must have git installed (currently the only supported scm) because they will clone the url repos (off-site for now) and squash the filesystem.

## Nodes
Beehive is a distributed system. You can add multiple nodes in the router. The node_manager handles the node connections.

Viewing the nodes is as easy as a query as well:

    curl -i beehive.com:8080/nodes

To add a new node, as mentioned above, start a node with the seed value from the start script:
  
    ./start_beehive.sh -s 'router@my-other-machine.com'

To add an existing node to a cluster, you can set the seed with:

    node_manager:set_seed(OtherNodePid).

## Users

Beehive has basic support for user accounts. The root user account information is:
    username: root@getbeehive.com
    password: test


It is strongly recommended that you delete this user as soon as you create your own (you must log in once with this user to create a new user).

Certain requests require an authenticated user. To remove the root@getbeehive.com user, we must authenticate. To authenticate, you must first get a token. This is achieved by submitting a request, such as:

    curl -i -XPOST -d"{\"email\":\"root@getbeehive.com\", \"password\": \"test\"}" http://beehive.com:8080/auth

This will return a tuple that will look like:
  
    {"user":"root@getbeehive.com","token":"f24e53e38dfb380066ea166f1844cf19"}

Subsequent requests that require authentication should attach this token onto the data.
    
Of course, it would be wise to add another admin user first. To add an admin level user, use the level 1 and ass, such as below

    curl -i -XPOST -d"{\"email\":\"arilerner@mac.com\", \"password\":\"myuniquepassword\", \"level\":\"1\", \"token\":\"f24e53e38dfb380066ea166f1844cf19\"}" beehive.com:8080/users/new

So, to remove the root@getbeehive.com user, we can then issue a request like:

    curl -i -XDELETE -d"{\"email\":\"root@getbeehive.com\", \"token\":\"f24e53e38dfb380066ea166f1844cf19\"}" beehive.com:8080/users    

And there you go, you have a custom authenticat-able user at your disposal.

Advanced
===

## Custom event handlers

The core functionality of Beehive is event-driven. It supports user defined callbacks as well. To hook into the beehive architecture, you will have to write a custom handler that exports the function: handle_event/1. And example of this custom event handler might look something like this:


    -module (my_callback_handler).
    -include ("/path/to/include/beehive.hrl").

    -export ([handle_event/1]).

    handle_event({bee, ready, Bee}) ->
      io:format("The bees: ~p is ready~n", [Bee#bee.id]);

    handle_event(_) -> ok.

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

    ./scripts/start_beehive.sh -a /path/to/custom_callback_module -c custom_callback_module

## Custom backend strategies

Backends are chosen via a strategy. There are baked in strategies that can be used. Those are described in the file: [src/mesh/router/backend_strategies.erl](https://github.com/auser/beehive/blob/master/lib/erlang/src/mesh/router/bee_strategies.erl).

If needed, custom strategies can be written to describe more complicated descriptions. To do so, write a custom handler, for instance:

    -module (custom_bee_picker).
    -include ("/path/to/include/beehive.hrl").
    -export([custom_chooser/1]).
    
    custom_chooser(Bees) ->
      hd(Bees).
      
This is clearly a dummy handler as it will choose the top of the bees, but it illustrates how to build a bee picker. To start with this bee picker at the top of the list:

    ./scripts/start_beehive.sh -a /path/to/custom_callback_module -q channel_chooser
    
Now, any application that has the routing_param set to channel_chooser will use the custom_chooser module.

    curl -i -XPOST -d"{\"token\":\"tokenofauthorizeduser\", \"name\":\"picky_app\", \"routing_param\":\"custom_chooser\", \"bee_picker\":\"custom_bee_picker\"}" beehive.com:8080/apps/new


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
  * Add token expiry to the user tokens
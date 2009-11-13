Beehive
===

## What is Beehive?

Beehive is an application-level, service as a service framework. It makes it easy to deploy an application into a multi-tenant environment, such as the clouds.

How it works
===

The incredibly basic architecture diagram of beehive looks like:

    Distributed Routing layer
    -------------------------
      |         |       |
    Backend   Backend Backend

The distributed routing layer, written in erlang uses [Mnesia](http://ftp.sunet.se/pub//lang/erlang/doc/apps/mnesia/index.html), the distributed database management system intelligently routes requests across the backends. The router currently can handle http requests. Because Beehive was written with the intention of being extensible it can be extensible to other protocols. 

It handles pending connections seamlessly and allows for streaming connections. It also keeps track of statistical data available through a web interface. The router itself has a RESTful interface for adding backends, which don't even need to sit inside the Beehive network. This can be useful for putting the router in front of a personal cluster (such as [Eucalyptus](http://www.eucalyptus.com/)) and expanding to the cloud environment (such as [EC2](http://aws.amazon.com/ec2/)) without having to change a line of code. 

Beehive keeps track of the available backends for the known applications. 

---
For more information about each particular part of the project, there are READMEs in each of the appropriate directories.
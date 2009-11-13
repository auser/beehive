Beehive
===

### What is Beehive?

Beehive is an application-level, service as a service framework. It makes it easy to deploy an application into a multi-tenant environment, such as the clouds.

How it works
===

The incredibly basic architecture diagram of beehive looks like:
<pre><code>
  Distributed Routing layer
    |         |       |
  Backend   Backend Backend
</code></pre>

The distributed routing layer, written in erlang uses [Mnesia](http://ftp.sunet.se/pub//lang/erlang/doc/apps/mnesia/index.html), the distributed database management system intelligently routes requests across the backends. The router currently can handle http requests. Because Beehive was written with the intention of being extensible it can be extensible to other protocols. 

It handles pending connections seamlessly and allows for streaming connections. It also keeps track of statistical data available through a web interface. 
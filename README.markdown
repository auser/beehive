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

The distributed routing layer, written in erlang uses [Mnesia](http://ftp.sunet.se/pub//lang/erlang/doc/apps/mnesia/index.html), the distributed database management system intelligently routes requests across the bees. The router currently can handle http requests. Because Beehive was written with the intention of being extensible it can be extensible to other protocols. 

It handles pending connections seamlessly and allows for streaming connections. It also keeps track of statistical data available through a web interface. The router itself has a RESTful interface for adding bees, which don't even need to sit inside the Beehive network. This can be useful for putting the router in front of a personal cluster (such as [Eucalyptus](http://www.eucalyptus.com/)) and expanding to the cloud environment (such as [EC2](http://aws.amazon.com/ec2/)) without having to change a line of code. 

Beehive keeps track of the available bees for the known applications. 

Beehive has an event system that allows for notifications along the system in a nonblocking manner. This way system events, statistic gathering log events can all be handled without affecting the performance of the router, which is tuned for speed. 

---

For more information about each particular part of the project, there are READMEs in each of the appropriate directories.

  * [Erlang](http://github.com/auser/beehive/tree/master/lib/erlang/)
  * [Ruby](http://github.com/auser/beehive/tree/master/lib/ruby/)
  * [Shell](http://github.com/auser/beehive/tree/master/lib/shell)

---

### Note on Patches/Pull Requests
 
  * Fork the project.
  * Make your feature addition or bug fix.
  * Add tests for it. This is important so I don't break it in a
    future version unintentionally.
  * Commit, do not mess with rakefile, version, or history.
    (if you want to have your own version, that is fine but
     bump version in a commit by itself I can ignore when I pull)
  * Send me a pull request. Bonus points for topic branches.

### Copyright

Copyright (c) 2009 Ari Lerner. See LICENSE for details.

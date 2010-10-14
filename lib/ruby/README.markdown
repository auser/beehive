beehive-client gem
===

A ruby command line client library for beehive.

Commands
===

apps                List Apps
bees                List Bees
create              Create a new app
create_bee          Create a new bee
register            Register a new user
restart             Restart an app
help                Display this screen
---------------------------------------
All commands support a -h flag for usage details


.beehive file
===

The gem supports a .beehive file in your home directory.
It expects a yaml format, and allows you to set user, password and
host fields.  Setting these in the .beehive config file negates the
need for command arguments of the same name.

Example:

    ---
    host: myhost.com:4999
    user: auser
    password: alsomyemailpassword


Gem Dependencies
===

 * json_pure
 * rest-client

Gem todos:
===

* tests
* more detailed help related to flags, etc.

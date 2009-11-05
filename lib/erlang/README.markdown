Router
===

## Proxy
app_srv:add_backend({"streaming",{127,0,0,1}, 5001}).
app_srv:add_backend({"srdves3", "services.speak4it.com", 80}).
app_srv:get_state().


event_manager:notify({info, "hi"}).

## Add an application

## Via REST:

  curl -i -XPOST -d"{\"name\":\"beehive.com\"}" beehive.com:8080/new
  curl -i -XPOST -d"{\"name\":\"applebees.com\"}" beehive.com:8080/new

Viewing the list of supported apps:

  curl -i beehive.com:8080/status

TODO
===
  * Consider double-storage in router_srv with dynos and backends
  * Add ets/mnesia storage to the front-end servers
  * Add more than just http servers (abstract the proxying protocols)
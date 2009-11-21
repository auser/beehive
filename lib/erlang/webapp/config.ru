$:.unshift(File.dirname(__FILE__))
# Stdlib
require 'rubygems'
require 'haml'

require 'rubygems'
require 'sinatra/base'
require 'haml/util'
require 'haml/engine'
require 'compass'

# Beehive
require 'beehive'

# use Rack::Static,
#   :root => "public",
#   :urls => ["/images",
#             "/javascripts"]

run BeehiveApp
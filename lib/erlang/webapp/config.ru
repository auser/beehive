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

run BeehiveApp
$:.unshift(File.dirname(__FILE__)+"/beehive")

%w(connection cli).each do |lib|
  require "#{lib}"
end

module Beehive
  include Beehive::Connection
end
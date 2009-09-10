$:.unshift(File.dirname(__FILE__)+"/beehive")

%w(askable colors).each do |lib|
  require File.dirname(__FILE__)+"/beehive/mixins/#{lib}"
end

module Beehive
end
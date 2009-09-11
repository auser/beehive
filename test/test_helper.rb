require 'rubygems'
require 'test/unit'

$LOAD_PATH.unshift(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'lib', 'ruby'))
require 'beehive'

class Test::Unit::TestCase
end

# Helpers
def fixtures_dir
  "#{::File.dirname(__FILE__)}/fixtures"
end

require 'rubygems'
require 'test/unit'
require 'yajl/json_gem'

require "#{File.dirname(__FILE__)}/../lib/ruby/beehive/mixins/rest"

class VerifyCloud < Test::Unit::TestCase
  include Beehive::Rest
  attr_reader :host, :token
  
  def setup
    @host="getbeehive.com"
    # @host="beehive.com:8080"
    @token = get_token("root@getbeehive.com", 'test')
  end
  
 def test_ok
   # this is just to suppress warning messages from test/unit
   assert true
 end
 
 def test_get_basics
  got = get('/')
  assert !got.empty?
  assert_equal got['beehive'], ["apps", "nodes", "bees", "stats"]
 end
 
 def test_get_bees
   #TODO: change naming from nodes to bees
  got = get("/nodes")
  %w(routers storage bees).each{|thing| assert_non_empty_enumerable got[thing]}
  assert !got['bees'].empty?
 end
  
 def test_list_users
   got = get("/users")
   assert_non_empty_enumerable got['users']
   assert_equal got['users']["root@getbeehive.com"], {"level" => "1"}
 end

 def get_token(email, password)
   # curl -XPOST -d"{\"email\":\"$EMAIL\", \"password\": \"$PASSWORD\"}" http://getbeehive.com:8080/auth
   post("/auth", {:email => email, :password => password})['token']
 end
  
 def test_create_user
   # curl -XPOST -d"{"email":"newuser@example.com", "password":"testsecret", "level":"1", "token":"$TOKEN"}" http://getbeehive.com:8080/users/new
   user = post("/users/new", {:email => 'albert@example.com', :password=>'realtivity', :token => token})
   assert_equal user["user"], 'albert@example.com'
   assert get('/users')['users']['albert@example.com']
 end
 
 def test_authenticate
 end
 
 def test_list_apps
 end
 
 def test_can_only_list_my_apps
 end
 
 def test_create_apps
  
 end
 
 private
 def assert_non_empty_enumerable(thing)
   assert !thing.empty?
   assert_respond_to thing, :each
 end

end
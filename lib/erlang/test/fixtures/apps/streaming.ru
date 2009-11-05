require 'rubygems'
require 'rack'

class Beep
  attr_reader :data
  def initialize(data)
    @data = data
  end
  def each    
    1000.times do |i|
      yield "<h3>#{i}</h3>"
      100.times {|a| yield "#{a} " }
    end
  end
end

class Streamer
  def self.call(env)
    req = Rack::Request.new(env)
    beep = Beep.new(req.body)
    [200, {'Content-Type' => 'text/html'}, beep]
  end
end

puts "RUNING!!!"
run Streamer
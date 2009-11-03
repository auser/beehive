require 'rubygems'
require 'rack'

class Beep
  attr_reader :data
  def initialize(data)
    @data = data
  end
  def each
    File.open("/Users/auser/.tsung/log/20091023-00:14/tsung.log", "rb") do |file|
      while part = file.read(4)
        yield part
      end
    end
    
    1000.times do |i|
      100.times {|a| yield "i: #{i + a}\t" }
    end
    # 
    # puts "DONE!"
  end
end

class Streamer
  def self.call(env)
    req = Rack::Request.new(env)
    beep = Beep.new(req.body)
    [200, {'Content-Type' => 'text/plain'}, beep]
  end
end

run Streamer
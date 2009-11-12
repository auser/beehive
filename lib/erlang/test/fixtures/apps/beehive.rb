require "rubygems"
require "yaml"
require 'sinatra/base'

class Beehive < Sinatra::Base
  get "/" do
   "Request: #{request.host}"
  end

  get '/status' do
    O = `curl http://beehive.com:8080/stats`
  end

  get "/keepalive" do
    headers['Cache-Control'] = 'max-age=60, must-revalidate'
    headers['Connection'] = 'keep-alive'

    "<pre>#{request.to_yaml}</pre>"
  end

  post "/asr" do
    "#{request.to_yaml}"
  end

  post "/hi" do
    "<pre>#{request.body.read}</pre>"
  end

  get "/favicon.ico" do
    ""
  end
end
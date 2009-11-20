require "rubygems"
require "yaml"
require 'sinatra/base'

class Beehive < Sinatra::Base
  get "/" do
   open("templates/index.html").read
  end

  get "/favicon.ico" do
    ""
  end
end
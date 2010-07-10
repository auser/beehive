require 'rubygems'
require 'sinatra'
require "#{File.dirname(__FILE__)}/app"

run Sinatra::Application

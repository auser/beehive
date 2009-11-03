#!/bin/sh

APP_NAME=$1
PORT=$2
echo "Mount a fake dyno at $APP_NAME" >> /tmp/mounted

echo "require 'rubygems'
require 'sinatra'
get '/' do
  'hello from port $APP_NAME:$PORT'
end" > /tmp/$APP_NAME.$PORT.rb

ruby /tmp/$APP_NAME.$PORT.rb -p $PORT 2>&1 > /tmp/$APP_NAME.$PORT.log
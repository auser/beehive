module Beehive
  
  class Application < Sinatra::Base
    set :views, "#{File.dirname(__FILE__)}/views"
    
    # don't like this at all
    # reopen classes everytime... yucky
    Dir["app/routes/*.rb"].each do |f| 
      require "#{f}"
    end
  end
  
end
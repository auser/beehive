require "rubygems"
require 'rest_client'
require "yaml"
require "yajl/json_gem"
require "pp"

module Beehive
  module Rest

     # REST Methods
     def get(path)
       r = RestClient.get("http://#{host}/#{path}")
       JSON.parse(r) # todo - http basic auth?
     end

     def post(path, params={})
       j = RestClient.post("http://#{host}/#{path}", params.to_json)
       handle_response(j)
     end

     def put(path, params={})
       j = RestClient.put("http://#{host}/#{path}", params.to_json)
       handle_response(j)
     end

     def delete(path)
       j = RestClient.delete("http://#{host}/#{path}")
       handle_response(j)
     end


     private

     def handle_response(resp)
       r = JSON.parse(resp)
        if r["error"]
          if r["error"] == "There was a problem authenticating"
            raise StandardError.new("
  There was an error authenticating
  Check your credentials
            ")
          else
            raise StandardError.new(r["error"])
          end
        else
          return r
        end
     end
  end
end

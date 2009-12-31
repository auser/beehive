Dir.glob(File.join(File.dirname(__FILE__), "..", "vendor", "gems", "*", "lib")).each do |lib|
  $LOAD_PATH.unshift(File.expand_path(lib))
end

require 'rest_client'
require "yaml"
require "yajl/json_gem"
require "pp"

module Beehive
  module Rest
     
     # REST Methods
     def get(path)
       r = RestClient.get("http://#{host}/#{path}")
       JSON.parse(r)
     end

     def post(path, params={})
       j = RestClient.post("http://#{host}/#{path}", params.to_json)
       r = JSON.parse(j)

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

     def put(path)

     end

     def delete(path)

     end
     
  end
end
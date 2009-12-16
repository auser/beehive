Dir.glob(File.join(File.dirname(__FILE__), "..", "vendor", "gems", "*", "lib")).each do |lib|
  $LOAD_PATH.unshift(File.expand_path(lib))
end

require 'rest_client'
require "yaml"
require "json"
require "pp"

module Beehive
  module Command
    
    class Base
      
      include Askable
      include Beehive::Connection
      
      attr_accessor :args, :host, :user, :keypair, :prefix
      
      def initialize(args=[])
        @args = args
      end
      
      def run
      end
      
      def parse_args(&block)
        options = {}
        opts = OptionParser.new {|opts|
          opts.on('-v', '--verbose') {|v| Beehive.verbose = true}
          opts.on('-e', '--very_verbose') {|v| Beehive.very_verbose = true}
          opts.on('-d', '--debugging') {|v| Beehive.debugging = true}
          opts.on('-r', '--very_debugging') {|v| Beehive.very_debugging = true}
          
          block.call(opts) if block
          
          opts.on("-o host", "--host host", 'The remote host') {|u| @host = u }
          opts.on("-u user", "--user user", 'The user') {|u| @user = u }
          opts.on("-p prefix", "--prefix prefix") {|u| @prefix = u }
          opts.on("-k keypair", "--keypair keypair") {|u| @keypair = Keypair.new(u) }

          opts.banner = "Usage: beehive [options]"
          opts.on( '-h', '--help', 'Display this screen' ) do
            puts opts
            exit
          end
        }.parse! @args
        
        opts
      end
      
      def tmp_dir
        @tmp_dir ||= "/tmp/beehive"
      end
      
      def self.inherited(base)
        base_classes << base
      end
      
      def self.base_classes
        @@base_classes ||= []
      end
      
      def user
        @user ||= config["user"]
      end
      
      def password
        @password ||= config["password"]
      end
      
      def host
        @host ||= config["host"]
      end
      
      # REST Methods
      def get(url)
        r = RestClient.get("http://#{host}/#{url}")
        JSON.parse(r)
      end
      
      def post(url, params={})
        j = RestClient.post("http://#{host}/#{url}", params.to_json)
        r = JSON.parse(j)

        if r["error"]
          raise r["error"] 
        else
          return r
        end
      end
      
      def put(url)
        
      end
      
      def delete(url)
        
      end
      
      def get_token
        creds = {"email" => user, "password" => password}
        r = post("auth", creds)
        
        unless @token = r["token"]
          raise r["error"] 
        end
      end
      
      private
      
      def config_file
        "#{ENV['HOME']}/.beehive"
      end
      
      def config(file=config_file)
        @config ||= File.exists?(file) ? YAML.load(open(file).read) : {}
      end
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/*"].each {|c| require c}
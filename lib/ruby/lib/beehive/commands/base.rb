module Beehive
  module Command
    
    class Base
      
      include Askable
      include Beehive::Connection
      include Beehive::Rest
      
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
          
          opts.on("-o host", "--host host", 'The remote host') {|u| @host = u }
          opts.on("-u user", "--user user", 'Your user') {|u| @user = u }
          opts.on("-p password", "--password password", 'Your password') {|u| @password = u }
          opts.on("-f prefix", "--prefix prefix") {|u| @prefix = u }
          opts.on("-k keypair", "--keypair keypair") {|u| @keypair = Keypair.new(u) }
          
          opts.banner = "Usage: beehive [options]"
          opts.on( '-h', '--help', 'Display this screen' ) do
            puts opts
            exit
          end
          
          block.call(opts) if block
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
      
      def get_token
        creds = {"email" => user, "password" => password}
        r = post("auth", creds.to_json)
        
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
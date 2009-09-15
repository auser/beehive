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
        
        @host       ||= @args[0]
        
        @user       ||= "root"
        @prefix     ||= "/opt/beehive"
        @keypair    ||= Keypair.new
        
        opts
      end
      
      def tmp_dir
        @tmp_dir ||= "/tmp/beehive"
      end
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/*"].each {|c| require c}
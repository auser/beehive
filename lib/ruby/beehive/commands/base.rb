module Beehive
  module Command
    
    class Base
      
      include Askable
      
      attr_accessor :args, :host, :user, :keypair
      
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
          
          block.call(opts)

          opts.on('-l location', '--location <string>', "The location on the remote server to store the filesystem", String)  do |val|
            options[:server_location] = val
          end

          opts.on('-s server', '--server <string>', "The remote server to bootstrap the git repos", String)  do |val|
            options[:remote_server] = val
          end

          opts.banner = "Usage: beehive [options]"
          opts.on( '-h', '--help', 'Display this screen' ) do
            puts opts
            exit
          end
        }.parse! @args
      end
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/*"].each {|c| require c}
module Beehive
  module Command
    
    class Setup < Base
      
      include Beehive::Connection
      
      attr_reader :server_ip, :options
      
      def initialize(server_ip, opts={})
        @server_ip = server_ip[0]
        @options = opts
        
        @user = options[:user] || "root"
        @prefix = options[:prefix] || "/opt/beehive"
        @keypair = options[:keypair] || File.expand_path("~/.ssh/id_rsa")
        @script_dir = Beehive.lib_dir + "/scripts"
      end
      
      def run(o={})
        raise Beehive::CommandError.new("You must pass an ip to setup") unless server_ip
        p ssh_options
      end
      
    end
    
  end
end
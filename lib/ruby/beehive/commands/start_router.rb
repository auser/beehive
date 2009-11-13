module Beehive
  module Command
    
    class StartRouter < Base
      
      def run
        parse_args do |opts|
          opts.on('-p port', '--port port', 'The port to start the router') {|p| @port = p}
        end
        
        @app_name ||= @args[0]
        
        puts <<-EOM
Starting the router...
Note, this should only be called in development mode as the system will start
the router without user intervention.
        EOM
        start_router
      end
      
      private
      
      def start_router
        Kernel.system("cd #{File.dirname(__FILE__)}/../../../erlang && make && ./scripts/start_router_dev.sh")
      end
            
    end
    
  end
end
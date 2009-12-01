module Beehive
  module Command
    
    class Create < Base
      
      attr_reader :app_name
      
      def self.description
        "Create a new app"
      end
            
      def run
        parse_args do |opts|
          opts.on('-n name', '--name name') {|n| @app_name = n}
          opts.on('-t type', '--type type') {|t| @type = t }
        end
        
        @app_name ||= @args[0]
        @type     ||= :rack
        
        puts <<-EOE
          host: #{host}
          user: #{user}
          password: #{password}
        EOE
      end
            
    end
    
  end
end
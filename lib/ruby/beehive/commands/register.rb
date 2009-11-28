module Beehive
  module Command
    
    class Register < Base
      
      attr_reader :app_name
      
      def self.description
        "Register a new user"
      end
      
      def run
        parse_args do |opts|
        end
        
        puts <<-EOE
          host: #{host}
          user: #{user}
          password: #{password}
        EOE
      end
            
    end
    
  end
end
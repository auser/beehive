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
        end
        
        get_token unless @token
        n = new_app
        
        puts <<-EOE
          host: #{host}
          user: #{user}
          password: #{password}
          #{n}
        EOE
      end
      
      def new_app
        r = post("apps/new", {  "name" => @app_name,
                                "token" => @token })
      end
            
    end
    
  end
end
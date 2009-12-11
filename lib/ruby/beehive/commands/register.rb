module Beehive
  module Command
    
    class Register < Base
      
      attr_reader :app_name
      
      def self.description
        "Register a new user"
      end
      
      def run
        parse_args do |opts|
          opts.on("-m email", "--email email", 'User\'s email') {|u| @new_email = u }
          opts.on("-a password", "--password password") {|u| @new_password = u }
          opts.on("-l level", "--level level") {|u| @new_level = u }
        end
        
        unless @token
          get_token
        end
        
        r = new_user
      
        puts <<-EOE
          host: #{host}
          user: #{user}
          password: #{password}
          new_email: #{@new_email}
          new_password: #{@new_password}
          new_level: #{@new_level}
          #{r}
        EOE
      end
       
      def new_user        
        r = post("users/new", { "email" => @new_email, 
                                "password" => @new_password,
                                "level" => @new_level,
                                "token" => @token} )
        raise r["error"] if r["error"]
      end
           
    end
    
  end
end
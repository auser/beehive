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
          opts.on( '-h', '--help', 'Display this screen' ) do
            puts <<-EOE

Usage:
Register a new user. Use this to register a new user for your own beehive.

            EOE
            puts opts
            exit
          end
        end

        get_token
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
        r = post("users/new.json",
                 { "email" => @new_email,
                   "password" => @new_password,
                   "level" => @new_level,
                   "token" => @token} )
      end

    end

  end
end

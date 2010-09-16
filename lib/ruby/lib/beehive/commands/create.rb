module Beehive
  module Command

    class Create < Base

      attr_reader :app_name

      def self.description
        "Create a new app"
      end

      def run
        parse_args do |opts|
          opts.on('-g git_url', '--git git_url', 'Git repos to pull from') {|n| @repo_url = n}
          opts.on('-n name', '--name name', 'Optional unique name') {|n| @app_name = n}
        end

        get_token unless @token
        n = JSON.parse(new_app)

        puts <<-EOE
          host: #{host}
          user: #{user}
          password: #{password}
          name: #{n}
        EOE
      end

      def new_app
        params = {"repo_url" => @repo_url,"token" => @token }
        params["name"] = @name if @name
        r = post("apps.json", params)
      end

    end

  end
end

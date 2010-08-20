module Beehive
  module Command

    class Restart < Base

      attr_reader :app_name

      def self.description
        "Restart an app"
      end

      def run
        parse_args do |opts|
          opts.on('-n name', '--name name') {|n| @app_name = n}
        end

        get_token unless @token
        n = restart_app
        p n
      end

      def restart_app
        post("apps/#{@app_name}/deploy.json", { "token" => @token })
      end

    end

  end
end

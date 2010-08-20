module Beehive
  module Command

    class Apps < Base

      attr_reader :app_name

      def self.description
        "List Apps"
      end

      def run
        parse_args

        pp apps_list
      end

      def apps_list
        get("apps.json")
      end

    end

  end
end

module Beehive
  module Command

    class Bees < Base

      attr_reader :app_name

      def self.description
        "List Bees"
      end

      def run
        parse_args do |opts|
        end

        pp bees_list
      end

      def bees_list
        get("bees.json")
      end

    end

  end
end

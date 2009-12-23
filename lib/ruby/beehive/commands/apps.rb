module Beehive
  module Command
    
    class Apps < Base
      
      attr_reader :app_name
      
      def self.description
        "List Apps"
      end
            
      def run
        parse_args do |opts|
        end
        
        pp apps_list
      end
      
      def apps_list
        get("apps")
      end

    end
    
  end
end
module Beehive
  module Command
    
    class Base
      
      include Askable
      
      def initialize
      end
      
      def run(o={})
      end
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/*"].each {|c| require c}
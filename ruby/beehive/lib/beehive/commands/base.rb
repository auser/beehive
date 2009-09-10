module Beehive
  module Command
    
    class Base
      
      include Askable
      
      def initialize
      end
      
      def run(o={})
      end
      
      # class << self
      #   def name
      #     to_s.split("::")[-1]
      #   end
      #   
      #   def description
      #   end
      # 
      #   def all_commands
      #     @all_commands ||= []
      #   end
      # 
      #   def inherited(subclass)
      #     all_commands << subclass
      #   end
      # end
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/*"].each {|c| require c}
module Beehive
  module Command
    
    class Base
      
      include Askable
      
      def initialize(args=[])
      end
      
      def run(args=[], o={})
      end
      
      protected
      
      attr_accessor :host, :user, :keypair
      
    end
    
  end
end

Dir["#{File.dirname(__FILE__)}/*"].each {|c| require c}
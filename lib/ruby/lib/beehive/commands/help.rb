module Beehive
  module Command
    
    class Help < Base
      
      def run(o={})
        commands = build_help.map do |klass|
          "#{klass.to_s.top_class}          #{klass.description}"
        end.join("\n")
        colored_say o[:msg] if o[:msg]
        colored_say "<line>
<yellow>Beehive</yellow>
<line>
Commands

#{commands}
Help            Display this screen
"
      end
    
      def build_help
        Dir["#{File.dirname(__FILE__)}/*"].each do |lib|
          require lib
        end
        self.class.base_classes.reject do |klass|
          !klass.respond_to?(:description)
        end
      end
    end
  end
end
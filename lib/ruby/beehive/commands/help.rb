module Beehive
  module Command
    
    class Help < Base
      
      def run(o={})
        colored_say o[:msg] if o[:msg]
        colored_say "<line>
<yellow>Beehive</yellow>
<line>
Commands

Help                  Display this screen
"
      end
    end
    
  end
end
module Beehive
  module Command
    
    class Help < Base
      
      def run(o={})
        colored_say <<-EOE
<line>
 <yellow>Beehive</yellow>
<line>
Commands

Help                  Display this screen
        EOE
      end
      
    end
    
  end
end
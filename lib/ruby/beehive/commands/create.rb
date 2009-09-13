module Beehive
  module Command
    
    class Create < Base
      
      def run
        
        parse_args do |opts|
          opts.on('-n name', '--name name') {|n| @app_name = n}
        end
        
        @app_name ||= @args[0]
        
        ssh "#{@prefix}/bin/git-bare-setup.sh #{@app_name}"
      end
      
    end
    
  end
end
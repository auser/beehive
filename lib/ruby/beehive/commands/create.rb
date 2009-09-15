module Beehive
  module Command
    
    class Create < Base
      
      attr_reader :app_name
      
      def run
        parse_args do |opts|
          opts.on('-n name', '--name name') {|n| @app_name = n}
          opts.on('-t type', '--type type') {|t| @type = t }
        end
        
        @app_name ||= @args[0]
        @type     ||= :rack
        
        ssh "#{@prefix}/bin/git-bare-setup.sh #{@app_name} #{user}"
        build_templates
      end
      
      def build_templates
        require "erb"
        t = ERB.new(open(Beehive.templates_dir/"nginx_config.conf.erb").read).result(self.send(:binding))
        File.open(tmp_dir/"#{@app_name}.conf", "w") {|f| f << t }
        scp(:source => tmp_dir/"#{@app_name}.conf", :destination => "/etc/nginx/sites-available/#{@app_name}.conf")
        ssh "sudo ln -s /etc/nginx/sites-available/#{@app_name}.conf /etc/nginx/sites-enabled/#{@app_name}.conf && sudo /etc/init.d/nginx restart"
      end
      
    end
    
  end
end
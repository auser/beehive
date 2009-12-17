module Beehive
  module Command
    
    class CreateBee < Base
      
      attr_reader :app_name
      
      def self.description
        "Create a new bee"
      end
            
      def run
        parse_args do |opts|
          opts.on('-n name', '--name name') {|n| @app_name = n}
          opts.on('-s host', '--host host') {|h| @app_host = h}
          opts.on('-P port', '--port port') {|p| @app_port = p}
        end
        
        get_token unless @token
        n = new_bee
        
        # puts <<-EOE
        #   host: #{host}
        #   user: #{user}
        #   password: #{password}
        #   #{@app_name}
        #   #{@app_host}
        #   #{@app_port}
        # EOE
      end
      
      def new_bee
        r = post("bees/new", {  "app_name" => @app_name,
                                "host" => @app_host,
                                "port" => @app_port,
                                "token" => @token })
      end
            
    end
    
  end
end

# curl -i -XPOST -d"{\"app_name\":\"testapp\", \"host\":\"google.com\", \"port\":\"80\"}" localhost:8080/bees/new

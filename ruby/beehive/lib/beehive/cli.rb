module Beehive
  
  class Cli
    
    def self.run(argv, remote_server=nil)
      require 'optparse'

      options = {}      
      options[:remote_server] = remote_server if remote_server
      
      opts = OptionParser.new {|opts|
        opts.banner = "Usage: beehive [options]"
        
        opts.on('-l location', '--location <string>', "The location on the remote server to store the filesystem", String)  do |val|
          options[:server_location] = val
        end
        
        opts.on('-s server', '--server <string>', "The remote server to bootstrap the git repos", String)  do |val|
          options[:remote_server] = val
        end
        
        opts.on( '-h', '--help', 'Display this screen' ) do
            puts opts
            exit
          end
      }.parse! argv
      
      # Defaults
      options[:server_location] ||= "/var/beehive"
      
      ssh(options[:server_location], options)
      options
    end
    
  end
  
end
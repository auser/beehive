module Beehive
  
  class CommandError < StandardError; end
  
  module Cli
      
    def self.run(command, argv=[])
      require 'optparse'
      require 'commands/base'

      options = {}
      opts = OptionParser.new {|opts|        
        opts.on('-l location', '--location <string>', "The location on the remote server to store the filesystem", String)  do |val|
          options[:server_location] = val
        end
        
        opts.on('-s server', '--server <string>', "The remote server to bootstrap the git repos", String)  do |val|
          options[:remote_server] = val
        end
        
        opts.banner = "Usage: beehive [options]"
        opts.on( '-h', '--help', 'Display this screen' ) do
          puts opts
          exit
        end
      }.parse! argv
      
      # Defaults
      options[:server_location] ||= "/var/beehive"
      begin
        command_klass = Beehive::Command.const_get(command.capitalize).new(opts)
        command_klass.send(:run, options)
      rescue Beehive::CommandError => e
        Beehive::Command::Help.new.run(:msg => "<red>Error with #{command}: #{e}</red>\n")
      rescue NameError => e
        p [:exception, e]
        Beehive::Command::Help.new.run(:msg => "<red>Unknown command: #{command}</red>\n")
      rescue Exception => e
        p [:exception, e.inspect]
      end
    end
    
  end
  
end
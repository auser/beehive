module Beehive
  
  class CommandError < StandardError; end
  
  module Cli
      
    def self.run(command, argv=[])
      require 'optparse'
      require 'commands/base'
      
      begin
        run_command command, argv
      rescue Beehive::CommandError => e
        Beehive::Command::Help.new.run(:msg => "<red>Error with #{command}: #{e}</red>\n")
      rescue LoadError => e
        error "Unknown command. Run 'beehive help' for more information"
      rescue StandardError => e
        puts e
      rescue NameError => e
        # p [:exception, e.inspect]
        Beehive::Command::Help.new.run(:msg => "<red>Unknown command: #{command}</red>\n")
      rescue Exception => e
        # p [:exception, e.inspect]
      end
    end
    
    def self.run_command(command, argv)
      require "commands/#{command}"
      command_klass = Beehive::Command.const_get(command.camelcase).new(argv)
      command_klass.send(:run)
    end
    
    def self.error(msg)
      STDERR.puts(msg)
      exit 1
    end
    
  end
  
end
module Beehive
  
  class CommandError < StandardError; end
  
  module Cli
      
    def self.run(command, argv=[])
      require 'optparse'
      require 'commands/base'
      
      begin
        command_klass = Beehive::Command.const_get(command.capitalize).new(argv)
        command_klass.send(:run)
      rescue Beehive::CommandError => e
        Beehive::Command::Help.new.run(:msg => "<red>Error with #{command}: #{e}</red>\n")
      rescue NameError => e
        p [:exception, e.inspect]
        Beehive::Command::Help.new.run(:msg => "<red>Unknown command: #{command}</red>\n")
      rescue Exception => e
        p [:exception, e.inspect]
      end
    end
    
  end
  
end
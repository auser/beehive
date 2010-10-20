module BeehiveClient

  class CommandError < StandardError; end

  require 'beehive_client/commands/base'

  module Cli

    def self.run(command, argv=[])

      begin
        run_command command, argv
      rescue BeehiveClient::CommandError => e
        BeehiveClient::Command::Help.new.
          run(:msg => "<red>Error with #{command}: #{e}</red>\n")
      rescue Exception => e
        p [:exception, e.inspect]
      end
    end

    def self.run_command(command, argv)

      if command_class = Command::Base.commands[command]
        command_class.new(argv).run
      else
        Command::Help.new.
          run(:msg => "<red>Unknown command: #{command}</red>")
      end
    end

    def self.error(msg)
      STDERR.puts(msg)
      exit 1
    end

  end

end

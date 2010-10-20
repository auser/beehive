module BeehiveClient
  module Command

    class Help < Base

      def run(o={})
        commands = build_help.map do |name, klass|
          "#{name.ljust(20)}#{klass.description}"
        end.join("\n")
        colored_say o[:msg] if o[:msg]
        colored_say "<line>
<yellow>BeehiveClient</yellow>
<line>
Commands

#{commands}
help                Display this screen
<line>
All commands support a -h flag for usage details
"
      end

      def build_help
        self.class.commands.select {|k,v| v.respond_to?(:description) }
      end

      def colored_say str
        out = Colors.process(str)
        puts out
        Colors.reset!
      end

    end
  end
end

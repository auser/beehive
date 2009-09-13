require "open3"

module Beehive
  module Connection
    
    attr_reader :host, :user
    
    def run(commands, o={})
      ssh(commands)
    end
    
    # Simply shell out and call ssh, simple, reliable and fewest dependencies, but slow
    def ssh( commands=[], extra_ssh_ops={})
      commands = commands.compact.join(' && ') if commands.is_a?(Array)
      if commands.empty?
        #TODO: replace this with a IO.popen call with read_nonblocking to show progress, and accept input
        Kernel.system(ssh_string(extra_ssh_ops))
      else
        system_run(ssh_string(extra_ssh_ops)+"'#{commands}'")
      end
    end
    
    def ssh_string(extra_ssh_ops={})
      "ssh #{user}@#{host} #{ssh_options(extra_ssh_ops)} "
    end
    
    # Take a hash of options and join them into a string, combined with default options.
    # Default options are -o StrictHostKeyChecking=no -i keypair.full_filepath -l user
    # {'-i'=>'keyfile, '-l' => 'fred' } would become
    # "-i keyfile -o StrictHostKeyChecking=no -i keypair.to_s -l fred"
    def ssh_options(opts={})
      o = {"-i" => keypair.full_filepath,
           "-o" =>"StrictHostKeyChecking=no"
           }.merge(opts)
      o.collect{ |k,v| "#{k} #{v}"}.join(' ')
    end
    
    def rsync( opts={} )
      raise StandardError.new("You must pass a :source=>uri option to rsync") unless opts[:source]
      destination_path = opts[:destination] || opts[:source]
      rsync_opts = opts[:rsync_opts] || '-va'
      cmd_string =  "rsync -L -e 'ssh #{ssh_options}' #{rsync_opts} #{opts[:source]}  #{user}@#{host}:#{destination_path}"
      ddputs "Running rsync with: #{cmd_string}"
      out = system_run(cmd_string)
      out
    end
    
    def scp(opts={})
      source = opts[:source]
      destination_path = opts[:destination] || opts[:source]
      raise StandardError.new("You must pass a local_file to scp") unless source
      scp_opts = opts[:scp_opts] || {}
      cmd_string = "scp #{ssh_options(scp_opts)} #{source} #{user}@#{host}:#{destination_path}"
      out = system_run(cmd_string)
      out
    end
    
    # Determine the os
    # Default to ubuntu
    # Send the determine_os.sh script to the node and run it remotely
    def determine_os
      o = ssh("/bin/sh #{@prefix}/determine_os.sh").chomp
      o.empty? ? :ubuntu : o
    end
    
    private
    
    # Execute command locally.
    # This method is mainly broken out to ease testing in the other methods
    # It opens the 3 IO outputs (stdin, stdout, stderr) and print the output out
    # as the command runs, unless the quiet option is passed in
    def system_run(cmd, o={})
      opts = {:quiet => false, :sysread => 1024}.merge(o)
      buf = ""
      ddputs("Running command: #{cmd}")
      Open3.popen3(cmd) do |stdout, stdin, stderr|
        begin
          while (chunk = stdin.readpartial(opts[:sysread]))
            buf << chunk
            unless chunk.nil? || chunk.empty?
              $stdout.write(chunk) if debugging? || verbose?
            end
          end
          err = stderr.readlines
          $stderr.write_nonblock(err)
        rescue SystemCallError => error
          $stderr.write_nonblock(stderr)
        rescue EOFError => error
           # nothing
        end
      end
      buf
    end
    
  end
end
require "fileutils"
module Beehive
  module Command
    
    class Setup < Base
      
      include Beehive::Connection
      
      attr_reader :options
      
      def run
        parse_args do |opts|
          opts.on("-o host", "--host host") {|u| @host = u }
          opts.on("-u user", "--user user") {|u| @user = u }
          opts.on("-p prefix", "--prefix prefix") {|u| @prefix = u }
          opts.on("-k keypair", "--keypair keypair") {|u| @keypair = Keypair.new(u) }
        end
        
        @host ||= @args[0]
        
        @user       ||= "root"
        @prefix     ||= "/opt/beehive"
        @keypair    ||= Keypair.new
        @script_dir = Beehive.lib_dir + "/shell"
        
        # Build the directories
        build_directory_structure
        rsync_and_setup_remote_directories
        cleanup unless debugging?
      end
      
      private
      
      def build_directory_structure
        FileUtils.mkdir_p tmp_dir
        required_directories.each do |dir|
          FileUtils.mkdir_p tmp_dir/dir
        end
        FileUtils.cp_r @script_dir/".", tmp_dir/"bin"
      end
      
      def required_directories
        %w(bin mnt repos squashed_fs src tmp logs)
      end
      
      def rsync_and_setup_remote_directories
        o = ssh("if [ -d #{@prefix} ]; then echo \"\"; else  echo \"no\"; fi").chomp
        if o == "no"
          @pass = ask "sudo password: "
          ssh "sudo mkdir -p #{@prefix} && sudo chown #{@user} #{@prefix}"
        end
        rsync(:source => tmp_dir, :destination => "/opt")
        @os = determine_os
        p @os
      end
      
      def cleanup
        FileUtils.rm_rf tmp_dir
      end
      
      def tmp_dir
        @tmp_dir ||= "/tmp/beehive"
      end
      
    end
    
  end
end
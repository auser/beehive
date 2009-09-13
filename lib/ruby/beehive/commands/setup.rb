require "fileutils"
module Beehive
  module Command
    
    class Setup < Base
      
      attr_reader :options
      
      def run
        parse_args
        
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
        ssh "sudo grep ^%sudo /etc/sudoers || echo \"%sudo ALL=NOPASSWD: ALL\" | sudo tee -a /etc/sudoers"
        rsync(:source => tmp_dir, :destination => "/opt")
        ssh "sudo #{install_required_software_command}"
      end
      
      def install_required_software_command
        case determine_os
        when :ubuntu
          "apt-get install -y build-essential curl git-core squashfs-tools"
        end
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
require "fileutils"
module Beehive
  module Command
    
    class Setup < Base
      
      attr_reader :options
      
      def run
        parse_args do |opts|
          opts.on("--no-nginx") {|n| @no_nginx = true }
        end
        
        @script_dir = Beehive.lib_dir + "/shell"
        @erlang_dir = Beehive.lib_dir + "/erlang"
        
        # Build the directories
        build_directory_structure
        setup_ssh_login_with_keypair_if_needed
        rsync_and_setup_remote_directories
        setup_application
        cleanup unless debugging?
      end
      
      private
      
      def build_directory_structure
        FileUtils.mkdir_p tmp_dir
        required_directories.each do |dir|
          FileUtils.mkdir_p tmp_dir/dir
        end
        FileUtils.cp_r @script_dir/".", tmp_dir/"bin"
        FileUtils.cp_r @erlang_dir/".", tmp_dir/"src"/"erlang"/"client"
      end
      
      def required_directories
        %w(bin mnt repos squashed_fs src src/erlang tmp logs)
      end
      
      def setup_ssh_login_with_keypair_if_needed
        f = Kernel.system("ssh #{user}@#{host} 'echo \"hi\"' > /dev/null")
        unless f
          puts "Sending up keypair"
          @pass = ask "password: "
          scp(:source => keypair.public_key_path, :destination => "/tmp")
          ssh "mkdir -p ~/.ssh && cat /tmp/#{keypair.basename}.pub >> ~/.ssh/authorized_keys"
        end
      end
      
      def rsync_and_setup_remote_directories
        o = ssh("if [ -d #{@prefix} ]; then echo \"\"; else  echo \"no\"; fi").chomp
        if o == "no"
          @pass = ask "sudo password: "
          ssh "echo #{@pass} | sudo mkdir -p #{@prefix} && sudo chown #{@user} #{@prefix}"
        end
        ssh "sudo grep ^%sudo /etc/sudoers || echo \"%sudo ALL=NOPASSWD: ALL\" | sudo tee -a /etc/sudoers"
        rsync(:source => tmp_dir, :destination => "/opt")
        ssh "#{prefix}/bin/setup-host.sh #{user}"
        # ssh "sudo #{install_required_software_command}"
      end
      
      def install_required_software_command
        case determine_os
        when :ubuntu
          "apt-get install -y build-essential curl git-core squashfs-tools"
        end
      end
      
      def setup_application
        %w(nginx).each do |app|
          ssh "#{@prefix}/bin/setup-#{app}.sh #{user}" unless instance_variable_get("@no_#{app}")
        end
      end
      
      def cleanup
        FileUtils.rm_rf tmp_dir
      end
      
    end
    
  end
end
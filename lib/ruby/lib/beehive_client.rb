$:.unshift(File.dirname(__FILE__)+"/beehive_client")

%w(core mixins helpers).each do |ty|
  Dir[File.dirname(__FILE__)+"/beehive_client/#{ty}/*"].each {|lib| require lib }
end

module BeehiveClient

  class << self
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging

    def lib_dir
      @lib_dir ||= File.join(File.dirname(__FILE__), "..")
    end

    def templates_dir
      @templates_dir ||= File.join(File.dirname(__FILE__), "..", "templates")
    end

    def prefix(n=nil)
      @prefix ||= n ? n : "/opt/beehive"
    end
    def mount_base;   prefix/"mnt";         end
    def repos_base;   prefix/"repos";       end
    def squash_base;  prefix/"squashed_fs"; end
    def tmp_base;     prefix/"tmp";         end
  end

end

$:.unshift(File.dirname(__FILE__)+"/beehive")

%w(core mixins helpers).each do |ty|
  Dir[File.dirname(__FILE__)+"/beehive/#{ty}/*"].each {|lib| require lib }
end

module Beehive
  
  class << self
    attr_accessor :verbose, :very_verbose, :debugging, :very_debugging
    
    def lib_dir
      @lib_dir ||= File.join(File.dirname(__FILE__), "..")
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
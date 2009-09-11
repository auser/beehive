$:.unshift(File.dirname(__FILE__)+"/beehive")

%w(core mixins helpers).each do |ty|
  Dir[File.dirname(__FILE__)+"/beehive/#{ty}/*"].each {|lib| require lib }
end

module Beehive
  
  def self.lib_dir
    @lib_dir ||= File.join(File.dirname(__FILE__), "..")
  end
  
end
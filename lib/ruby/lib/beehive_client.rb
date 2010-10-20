%w(core mixins helpers).each do |ty|
  Dir[File.dirname(__FILE__)+"/beehive_client/#{ty}/*"].each {|lib| require lib }
end

module BeehiveClient
end

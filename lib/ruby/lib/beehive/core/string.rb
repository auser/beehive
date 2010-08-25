class String
  ##
  # @param o<String> The path component to join with the string.
  #
  # @return <String> The original path concatenated with o.
  #
  # @example
  #   "merb"/"core_ext" #=> "merb/core_ext"
  def /(o)
    File.join(self, o.to_s)
  end

  def top_class
    self.split("::")[-1]
  end

  # foo_bar => FooBar
  def camelcase
    gsub(/(^|_|-)(.)/) { $2.upcase }
  end

end

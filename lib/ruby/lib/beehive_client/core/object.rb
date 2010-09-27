class Object
  # MESSAGES
  # Debugging output helpers
  def vputs(m="")
    puts "[INFO] -- #{m}" if verbose?
  end
  def dputs(m="")
    puts "[DEBUG] -- #{m.is_a?(String) ? m : m.inspect}" if debugging?
  end
  def ddputs(m="")
    puts "[VERY DEBUG] -- #{m.is_a?(String) ? m : m.inspect}" if very_debugging?
  end
  def verbose?
    BeehiveClient.verbose ||= false
  end
  def very_verbose?
    (BeehiveClient.very_verbose ||= false)
  end
  def debugging?
    (BeehiveClient.debugging ||= false)
  end
  def very_debugging?
    (BeehiveClient.very_debugging ||= false)
  end
end

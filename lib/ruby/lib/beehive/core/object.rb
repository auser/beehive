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
    Beehive.verbose ||= false
  end
  def very_verbose?
    (Beehive.very_verbose ||= false)
  end
  def debugging?
    (Beehive.debugging ||= false)
  end
  def very_debugging?
    (Beehive.very_debugging ||= false)
  end
end

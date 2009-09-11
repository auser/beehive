require "#{File.dirname(__FILE__)}/../../test_helper"

Keypair.searchable_paths << fixtures_dir/"keys"

class KeypairTest < Test::Unit::TestCase
  def setup
    @keypair = Keypair.new(fixtures_dir/"keys"/"test_key")
  end
    
  def test_set_the_file_given_as_the_file_for_the_keypair
      assert_equal @keypair.filepath, fixtures_dir/"keys"/"test_key"
      assert_equal @keypair.full_filepath, File.expand_path(fixtures_dir/"keys"/"test_key")
      assert_equal @keypair.to_s, File.expand_path(fixtures_dir/"keys"/"test_key")
    end
    
    def test_have_the_content_of_the_file_available
      assert_equal @keypair.content, open(fixtures_dir/"keys"/"test_key").read
    end
    
    def test_have_the_basename_of_the_keypair
      assert_equal @keypair.basename, "test_key"
      assert_equal @keypair.filename, "test_key"      
    end
    
    def test_be_valid_if_it_s_600_permissions
      assert @keypair.valid?
    end
    
  def test_be_invalid_if_the_file_permissions_are_executable
    assert_raises StandardError do
      Keypair.new(fixtures_dir/"bad_perms_test_key").valid?
    end
  end  
end
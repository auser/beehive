pool "beehive" do
  
  cloud "router" do
    using :ec2
    instances 1
    image_id "ami-1515f67c"
    user 'ubuntu'
    elastic_ip "174.129.43.151"
    security_group do
      authorize :from_port => 22, :to_port => 22
      authorize :user_id => ENV["EC2_USER"], :group_name => "beehive-router"
      authorize :from_port => 80, :to_port => 80
      authorize :from_port => 4369, :to_port => 4369
      authorize :from_port => 8080, :to_port => 8080
    end
    user_data open("#{File.dirname(__FILE__)}/user-data/router.sh").read
  end
  
end
pool "beehive" do
  
  cloud "router" do
    using :ec2
    instances 1
    image_id "ami-4205e72b" #ebs Ubuntu 9.10 base server ami
    user 'ubuntu'
    elastic_ip "174.129.204.191"
    security_group do
      authorize :from_port => 22, :to_port => 22
      authorize :user_id => ENV["EC2_USER"], :group_name => "beehive-router"
      authorize :user_id => ENV["EC2_USER"], :group_name => "beehive-bees"
      authorize :from_port => 80, :to_port => 80
      authorize :from_port => 4369, :to_port => 4369
      authorize :from_port => 8080, :to_port => 8080
    end
    user_data open("#{File.dirname(__FILE__)}/user-data/router.sh").read
  end
  
  cloud "bees" do
    using :ec2
    instances 1
    image_id "ami-4205e72b"
    user 'ubuntu'
    security_group do
      authorize :from_port => 22, :to_port => 22
      authorize :user_id => ENV["EC2_USER"], :group_name => "beehive-router"
      authorize :from_port => 80, :to_port => 80
      authorize :from_port => 4369, :to_port => 4369
      authorize :from_port => 8080, :to_port => 8080
    end
    user_data open("#{File.dirname(__FILE__)}/user-data/bees.sh").read
  end
  
end
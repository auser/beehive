#!/bin/sh

sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev libopenssl-ruby
sudo apt-get install -y squashfs-tools
sudo apt-get autoremove -y

# So we can deploy thin and rack apps
sudo gem install rack thin --no-rdoc --no-ri
sudo gem install haml sinatra --no-rdoc --no-ri

echo "HwlloE0lrd" > ~/.erlang.cookie
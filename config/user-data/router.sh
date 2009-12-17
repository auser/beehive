#!/bin/sh

sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install ruby rubygems ruby-dev -y
sudo apt-get autoremove -y

# Making base beehive directories
sudo mkdir -p /opt/beehive

sudo cp ~/.bashrc ~beehive/.bashrc
echo "HwlloE0lrd" > /opt/beehive/.erlang.cookie
sudo chmod 600 /opt/beehive/.erlang.cookie

sudo chown beehive -R /opt/beehive
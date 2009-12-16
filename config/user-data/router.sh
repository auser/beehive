#!/bin/sh

cd /tmp
sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y erlang
sudo apt-get install ruby rubygems ruby-dev -y
sudo apt-get autoremove -y

ERL=$(which erl)

if [ -z "$ERL" ]; then
	wget http://www.erlang.org/download/otp_src_R13B03.tar.gz
	tar -zxf otp_src_R13B02-1.tar.gz
	cd otp_src_R13B02-1
	./configure  --enable-smp-support --enable-threads --enable-kernel-poll --enable-hipe --with-ssl
	make
	sudo make install
fi

# Beehive specific stuff
if [ $(sudo cat /etc/passwd | grep beehive | grep -v "#" | wc -l) -eq 0 ]; then
	sudo useradd -s /bin/bash -b /opt/beehive -d /opt/beehive -c "beehive user" -g users beehive;
fi

# Making base beehive directories
sudo mkdir -p /opt/beehive
sudo mkdir -p /opt/beehive/src
sudo mkdir -p /opt/beehive/bin

sudo cp ~ubuntu/.bashrc ~beehive/.bashrc
echo "HwlloE0lrd" > /opt/beehive/.erlang.cookie
sudo chmod 600 /opt/beehive/.erlang.cookie

sudo chown beehive -R /opt/beehive
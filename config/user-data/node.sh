#!/bin/sh

sudo apt-get update -y
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev libopenssl-ruby
sudo apt-get install -y squashfs-tools
sudo apt-get autoremove -y

# So we can deploy thin and rack apps
sudo gem install rack thin --no-rdoc --no-ri
sudo gem install haml sinatra --no-rdoc --no-ri

# Create as many loop back devices as we can
for i in $(seq 0 255); do
	sudo mknod -m0660 /dev/loop$i b 7 $i 2>&1 >/dev/null
done

echo "HwlloE0lrd" > ~/.erlang.cookie

ERL=$(which erl)
OTP_VERSION=otp_src_R13B03

cd /tmp
if [ -z "$ERL" ]; then
	wget http://www.erlang.org/download/$OTP_VERSION.tar.gz
	tar -zxf $OTP_VERSION.tar.gz
	cd $OTP_VERSION
	./configure  --enable-smp-support --enable-threads --enable-kernel-poll --enable-hipe --with-ssl
	make
	sudo make install
fi

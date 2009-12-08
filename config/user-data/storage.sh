#!/bin/sh

cd /tmp
sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y erlang
sudo apt-get install -y git git-core
sudo apt-get install -y squashfs-tools
sudo apt-get autoremove -y

wget http://www.erlang.org/download/otp_src_R13B03.tar.gz
tar -zxf otp_src_R13B02-1.tar.gz
cd otp_src_R13B02-1
./configure  --enable-smp-support --enable-threads --enable-kernel-poll --enable-hipe --with-ssl
make
sudo make install

echo "HwlloE0lrd" > ~/.erlang.cookie
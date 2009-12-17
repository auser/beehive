#!/bin/sh

# sudo apt-get install -y erlang-nox erlang-dev

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

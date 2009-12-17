#!/bin/sh

ERL=$(which erl)

cd /tmp
if [ -z "$ERL" ]; then
	wget http://www.erlang.org/download/otp_src_R13B03.tar.gz
	tar -zxf otp_src_R13B03-1.tar.gz
	cd otp_src_R13B03-t
	./configure  --enable-smp-support --enable-threads --enable-kernel-poll --enable-hipe --with-ssl
	make
	sudo make install
fi

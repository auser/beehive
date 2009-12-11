#!/bin/sh

GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
THIN_APP="$GEM_ENV/thin"

echo "thin $THIN_APP"
cd [[APP_HOME]]

sudo /usr/sbin/chroot [[APP_HOME]] /usr/bin/env -i \
         HOME=[[APP_HOME]]
         TERM=$TERM PS1='\u:\w\$ ' \
         HI="hi" \
				 $THIN_APP -R home/app/config.ru --log tmp/beehive.log --pid tmp/beehive-5001.pid --port [[PORT]] start
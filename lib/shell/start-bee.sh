#!/bin/sh

GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
GEM_PATHS=$(ruby -r rubygems -e "p Gem.path.join(':')")
THIN_APP="$GEM_ENV/thin"

echo "thin $THIN_APP"
cd [[APP_HOME]]


sudo /usr/sbin/chroot [[APP_HOME]] /usr/bin/env -i \
        HOME=[[APP_HOME]]
        TERM=$TERM PS1='\u:\w\$ ' \
        HI="hi" \
				PATH=$PATH:$GEM_ENV \
				APP_NAME=[[APP_NAME]] \
				GEM_PATH=[[APP_HOME]]/.gems:$GEM_PATHS \
				/bin/bash -c \
				"thin -R home/app/config.ru --log tmp/[[APP_NAME]].log --pid tmp/[[APP_NAME]]-[[PORT]].pid --port [[PORT]] start"
				
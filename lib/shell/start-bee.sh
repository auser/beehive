#!/bin/sh

GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
GEM_PATHS=$(ruby -r rubygems -e "p Gem.path.join(':')")
THIN_APP="$GEM_ENV/thin"

echo "thin $THIN_APP"
cd [[APP_HOME]]

OUT=$(/usr/sbin/chroot [[APP_HOME]] \
  /usr/bin/env -i \
  HOME=/ \
  HI="Hello world" \
  PATH=$PATH:$GEM_ENV \
  WHOAMI=[[APP_NAME]] \
  APP_NAME=[[APP_NAME]] \
  GEM_PATH=[[APP_HOME]]/.gems:$GEM_PATHS \
  /bin/su -m [[APP_NAME]] \
  /bin/bash -c \
  "thin -d -R home/app/config.ru --log tmp/[[APP_NAME]].log --pid tmp/[[APP_NAME]]-[[PORT]].pid --port [[PORT]] start")

if [ $? -neq 0 ]; then
  echo "error $OUT"
else
  echo "started true"
fi
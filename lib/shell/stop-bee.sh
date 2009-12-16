#!/bin/sh

echo "stopped true"
GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
THIN_APP="$GEM_ENV/thin"

echo "thin $THIN_APP"
cd [[APP_HOME]]

/usr/sbin/chroot [[APP_HOME]] \
  /usr/bin/env -i \
  HOME=/ \
  HI="Hello world" \
  PATH=$PATH:$GEM_ENV \
  WHOAMI=[[APP_NAME]] \
  APP_NAME=[[APP_NAME]] \
  GEM_PATH=[[APP_HOME]]/.gems:$GEM_PATHS \
  /bin/su -m [[APP_NAME]] \
  /bin/bash -c \
  "ps aux | grep thin | grep [[APP_NAME]] | awk '{print $2}' | xargs kill"
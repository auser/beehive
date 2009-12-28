#!/bin/sh

GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
GEM_PATHS=$(ruby -r rubygems -e "p Gem.path.join(':')")
THIN_APP="$GEM_ENV/thin"
HOST_IP=[[HOST_IP]]
APP_HOME=[[APP_HOME]]
APP_NAME=[[APP_NAME]]
PORT=[[PORT]]
SHA=[[SHA]]
START_TIME=[[START_TIME]]

echo "thin $THIN_APP"
cd $APP_HOME

START_COMMAND="$THIN_APP -d -R home/app/config.ru --log tmp/$APP_NAME.log --pid tmp/$PID_NAME --port $PORT start"

if [ -f $APP_HOME/home/app/start.sh ]; then
  START_COMMAND=$(cat $APP_HOME/home/app/start.sh)
fi

CMD="/usr/sbin/chroot $MOUNT_LOCATION \
  /usr/bin/env -i \
  HOME=/ \
  HI=\"Hello world\" \
  PATH=/usr/local/bin:/usr/bin:/bin:$GEM_ENV \
  WHOAMI=$APP_NAME \
  STARTED_AT=$START_TIME \
  APP_NAME=$APP_NAME \
  COMMIT_HASH=$SHA \
  GEM_PATH=$APP_HOME/.gems:$GEM_PATHS \
  /bin/su -m $APP_NAME \
  /bin/bash -c \
  \"$START_COMMAND\""

echo "start_command $CMD"
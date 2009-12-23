#!/bin/bash

PREFIX=$HOME
MOUNT_BASE=$PREFIX/mnt

# mount a bee!
APP_NAME=[[APP_NAME]]
# Fake the sha
SHA=[[SHA]]
PORT=[[PORT]]
MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME/$SHA/$(date +%H%M%S)
MOUNT_FILE=[[BEE_IMAGE]]
NEW_LOOP_DEVICE=$(comm -13 <(mount | grep /dev/loop | awk '{print $1}') <(ls /dev/loop*) | head -1)

# MESSAGES
COULD_NOT_ADD_USER=1
COULD_NOT_START_APP=2
COULD_NOT_MOUNT_APP=3
COULD_NOT_UNMOUNT_OLD_PROCESSES=4

# Grab the currently mounted filesystems
MOUNTED=$(mount | grep $APP_NAME | grep -v $SHA |  awk '{a[i++]=$0} END {for (j=i-1; j>=0;) print a[j--] }' | awk '{print $3}')
# Old mounts
OLD_LOOP_DEVICE=$(mount | grep $APP_NAME | grep -v $SHA | grep /dev/loop | awk '{print $1}')

if [ ! -d $MOUNT_LOCATION ]; then
	mkdir -p $MOUNT_LOCATION
fi

# Create chroot user
CHROOT_USER="$APP_NAME"

if [ $(sudo cat /etc/passwd | grep $CHROOT_USER | grep -v "#" | wc -l) -eq 0 ]; then
  useradd -s /bin/sh -d $MOUNT_LOCATION/./ -c "$APP_NAME user" -g users $CHROOT_USER;
	echo "added_user $CHROOT_USER"
else
  echo "user_exists $CHROOT_USER";
fi
if [ $? != 0 ]; then exit $COULD_NOT_ADD_USER; fi

mount $MOUNT_FILE $MOUNT_LOCATION -o ro -o loop
if [ $? != 0 ]; then exit $COULD_NOT_MOUNT_APP; fi
  
# Create a tmp directory
mkdir -p /tmp/$APP_NAME/$SHA
chown $CHROOT_USER /tmp/$APP_NAME/$SHA

# Bind mount the system
mount --bind /bin $MOUNT_LOCATION/bin -o ro
mount --bind /etc $MOUNT_LOCATION/etc -o ro
mount --bind /usr $MOUNT_LOCATION/usr -o ro
mount --bind /lib $MOUNT_LOCATION/lib -o ro
mount --bind /dev $MOUNT_LOCATION/dev -o ro
mount --bind /var $MOUNT_LOCATION/var -o ro
mount -t proc /proc $MOUNT_LOCATION/proc
mount --bind /tmp/$APP_NAME/$SHA $MOUNT_LOCATION/tmp -o rw

# If there is a lib64 directory 
if [ -d /lib64 ]; then
  sudo mount --bind /lib64 $MOUNT_LOCATION/lib64 -o ro
fi

# Run the application!
# This should be in a separate process, but... because we need
# to retain the OLD_PROCESSES pids and the old MOUNTED devices
# we'll put this in here
GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
GEM_PATHS=$(ruby -r rubygems -e "p Gem.path.join(':')")
THIN_APP="$GEM_ENV/thin"

echo "thin $THIN_APP"

PIDS_DIR=/tmp/$APP_NAME/$SHA
mkdir -p $PIDS_DIR
PID_NAME=$APP_NAME-$SHA-$PORT.pid

# Kill the previous thin process, just in case
if [ -f $PIDS_DIR/$PID_NAME ]; then
  cat $PIDS_DIR/$PID_NAME | xargs kill -9 2>&1 >/dev/null
  rm $PIDS_DIR/$PID_NAME
fi

START_COMMAND="$THIN_APP -d -R home/app/config.ru --log tmp/$APP_NAME.log --pid tmp/$PID_NAME --port $PORT start"

if [ -f $MOUNT_LOCATION/home/app/start.sh ]; then
  START_COMMAND=$(cat $MOUNT_LOCATION/home/app/start.sh)
fi

/usr/sbin/chroot $MOUNT_LOCATION \
  /usr/bin/env -i \
  HOME=/ \
  HI="Hello world" \
  PATH=$PATH:$GEM_ENV \
  WHOAMI=$APP_NAME \
  APP_NAME=$APP_NAME \
  COMMIT_HASH=$SHA \
  GEM_PATH=$MOUNT_LOCATION/.gems:$GEM_PATHS \
  /bin/su -m $APP_NAME \
  /bin/bash -c \
  "$START_COMMAND"

if [ $? != 0 ]; then exit $COULD_NOT_START_APP; fi

OLD_PROCESS_PIDS=$(ls $PIDS_DIR/*.pid | grep -v $SHA)
# Terminate the old bees
if [ ! -z "$OLD_PROCESS_PIDS" ]; then
  for p in $OLD_PROCESS_PIDS; do
    cat $p | xargs kill -9 2>&1 >/dev/null
    rm $p
  done
fi

if [ ! -z "$MOUNTED" ]; then
  for i in $MOUNTED; do
    sudo umount -l $i
  done
fi
if [ $? != 0 ]; then exit $COULD_NOT_UNMOUNT_OLD_PROCESSES; fi

echo "app_name $APP_NAME"
echo "path $MOUNT_LOCATION"
exit 0

#!/bin/bash

PREFIX=$HOME
MOUNT_BASE=$PREFIX/mnt

# mount a bee!
APP_NAME=[[APP_NAME]]
# Fake the sha
SHA=[[SHA]]
PORT=[[PORT]]
HOST_IP="[[HOST_IP]]"
APP_NAME=[[APP_NAME]]
START_TIME=[[START_TIME]]
MOUNT_FILE=[[BEE_IMAGE]]
MOUNT_LOCATION="$MOUNT_BASE/$APP_NAME/$SHA/$(date +%H%M%S%y%m%d)"

# MESSAGES
COULD_NOT_ADD_USER=1
COULD_NOT_START_APP=2
COULD_NOT_MOUNT_APP=3
COULD_NOT_UNMOUNT_OLD_PROCESSES=4

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

GEM_ENV=$(gem env | grep "EXECUTABLE DIRECTORY" | awk '{print $4}')
GEM_PATHS=$(ruby -r rubygems -e "print Gem.path.join(':')")
THIN_APP="$GEM_ENV/thin"

echo "thin $THIN_APP"
echo "path $MOUNT_LOCATION"

cd $MOUNT_LOCATION

START_COMMAND="$THIN_APP -R home/app/config.ru --log tmp/$APP_NAME.log --port $PORT start"

if [ -f $MOUNT_LOCATION/home/app/start.sh ]; then
  START_COMMAND=$(cat $MOUNT_LOCATION/home/app/start.sh)
fi

CMD="/usr/sbin/chroot $MOUNT_LOCATION \
  /usr/bin/env -i \
  HOME=/ \
  HI='Hello world' \
  PATH=/usr/local/bin:/usr/bin:/bin:$GEM_ENV \
  WHOAMI=$APP_NAME \
  STARTED_AT=$START_TIME \
  APP_NAME=$APP_NAME \
  COMMIT_HASH=$SHA \
  HOST_IP=$HOST_IP \
  LOCAL_PORT=$PORT \
  RACK_ENV=production \
  GEM_PATH=$MOUNT_LOCATION/.gems:$GEM_PATHS \
  /bin/su -m $APP_NAME \
  /bin/bash -c \
  '$START_COMMAND'"

echo "start_command $CMD"

# Right now, this umounts all... consider: grep -v $SHA again
STOPCMD="/bin/kill -9 [[PID]]; \
  MOUNTED=\$(mount | grep $APP_NAME | grep $SHA | awk '{a[i++]=\$3} END {for (j=i-1; j>=0;) print a[j--] }'); \
  for i in \$MOUNTED; do sudo umount \$i -f >/dev/null 2>&1; done"

echo "stop_command $STOPCMD"
exit 0
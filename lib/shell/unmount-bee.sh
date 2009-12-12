#!/bin/bash

PREFIX=/opt/beehive
MOUNT_BASE=$PREFIX/mnt

# mount a bee!
APP_NAME=[[APP_NAME]]
MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME
LOOP_DEVICE=/dev/$APP_NAME

# Unmount the device if it's already mounted
MOUNTED=$(mount | grep $APP_NAME | awk '{a[i++]=$3} END {for (j=i-1; j>=0;) print a[j--] }')
for i in $MOUNTED; do
  sudo umount $i -f >/dev/null 2>&1
done
sudo umount $LOOP_DEVICE >/dev/null 2>&1

if [ ! -e $LOOP_DEVICE ]; then
  mknod -m 600 $LOOP_DEVICE b 7 0
fi

if [ ! -d $MOUNT_LOCATION ]; then
	mkdir -p $MOUNT_LOCATION
fi

echo "unmounted true"
exit 0
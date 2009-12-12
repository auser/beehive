#!/bin/bash

PREFIX=/opt/beehive
MOUNT_BASE=$PREFIX/mnt

# mount a bee!
APP_NAME=[[APP_NAME]]
MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME
LOOP_DEVICE=/dev/$APP_NAME
MOUNT_FILE=[[BEE_IMAGE]]

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

# Create chroot user
CHROOT_USER="[[APP_NAME]]"

if [ $(sudo cat /etc/passwd | grep $CHROOT_USER | grep -v "#" | wc -l) -eq 0 ]; then
  useradd -s /bin/sh -d $MOUNT_LOCATION/./ -c "$APP_NAME user" -g users $CHROOT_USER;
	echo "added_user $CHROOT_USER"
else
  echo "user_exists $CHROOT_USER";
fi

# Mount it! loop=$LOOP_DEVICE
mount $MOUNT_FILE $MOUNT_LOCATION -t squashfs -o ro -o loop

# Create a tmp directory
mkdir -p /tmp/$APP_NAME

# Bind mount the system
mount --bind /bin $MOUNT_LOCATION/bin -o ro
mount --bind /etc $MOUNT_LOCATION/etc -o ro
mount --bind /usr $MOUNT_LOCATION/usr -o ro
mount --bind /lib $MOUNT_LOCATION/lib -o ro
mount --bind /dev $MOUNT_LOCATION/dev -o ro
mount --bind /var $MOUNT_LOCATION/var -o ro
mount -t proc /proc $MOUNT_LOCATION/proc
mount --bind /tmp/$APP_NAME $MOUNT_LOCATION/tmp -o rw
# Consider adding logs

# If there is a lib64 directory 
if [ -d /lib64 ]; then
  sudo mount --bind /lib64 $MOUNT_LOCATION/lib64 -o ro
fi

echo "app_name [[APP_NAME]]"
echo "path $MOUNT_LOCATION"
exit 0
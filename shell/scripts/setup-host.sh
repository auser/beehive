#!/bin/sh

# LOOP_NUM=$1
# 
# if [ -z $LOOP_NUM ]; then
#   LOOP_NUM=64
# fi
# 
# for ((i=8;i<$LOOP_NUM;i++)); do 
#   [ -e /dev/loop$i ] || sudo mknod -m 0600 /dev/loop$i b 7 $i; 
# done

USER=$(whoami)
GROUP=admin
PREFIX=/opt/beehive
# Mounts
MOUNT_BASE=$PREFIX/mnt
REPOS_BASE=$PREFIX/repos
SQUASH_BASE=$PREFIX/squashed_fs
SRC_BASE=$PREFIX/src
TMP_DIR=$PREFIX/tmp
DEVICE_BASE=$PREFIX/dev

# Make the base directories
if [ ! -d $PREFIX ]; then
  sudo mkdir -p $PREFIX
  sudo chown -R $USER $PREFIX
fi
if [ ! -d $MOUNT_BASE ]; then
  mkdir -p $MOUNT_BASE
fi
if [ ! -d $REPOS_BASE ]; then
  mkdir -p $REPOS_BASE
fi
if [ ! -d $SQUASH_BASE ]; then
  mkdir -p $SQUASH_BASE
fi
if [ ! -d $SRC_BASE ]; then
  mkdir -p $SRC_BASE
fi
if [ ! -d $TMP_DIR ]; then
  mkdir -p $TMP_DIR
fi
if [ ! -d $DEVICE_BASE ]; then
  mkdir -p $DEVICE_BASE
fi

# Install essential build tools
sudo apt-get install build-essential -y
sudo apt-get install curl -y

# Install git
sudo apt-get install git-core -y
# Install squashfs
sudo apt-get install squashfs-tools -y

# Setup users
sudo chgrp $GROUP /etc/fstab
sudo chmod 664 /etc/fstab

sudo usermod -a -G $GROUP $USER
SUDOER_STR="%$GROUP ALL=NOPASSWD:/bin/mount,/bin/umount"

echo "
  Make sure this is in your /etc/sudoers file:
  
  $SUDOER_STR
"
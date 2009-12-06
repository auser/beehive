#!/bin/bash

PREFIX=/opt/beehive
MOUNT_BASE=$PREFIX/mnt

# mount a bee!
APP_NAME=[[APP_NAME]]
MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME
LOOP_DEVICE=/dev/$APP_NAME
MOUNT_FILE=[[BEE_IMAGE]]

echo "mount $MOUNT_FILE $MOUNT_LOCATION -t squashfs -o ro -o loop=$LOOP_DEVICE" > /tmp/out
OUT=$(mount $MOUNT_FILE $MOUNT_LOCATION -t squashfs -o ro -o loop=$LOOP_DEVICE)

echo "path $MOUNT_LOCATION"
#!/bin/sh

APP_NAME=[[APP_NAME]]
SHA=[[SHA]]

PIDS_DIR="/tmp/$APP_NAME/$SHA"
OLD_PROCESS_PIDS=$(ls $PIDS_DIR/*.pid | grep $SHA)
MOUNTED=$(mount | grep $APP_NAME | grep $SHA |  awk '{a[i++]=$0} END {for (j=i-1; j>=0;) print a[j--] }' | awk '{print $3}')

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

echo "stopped true"
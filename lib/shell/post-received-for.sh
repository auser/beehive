#!/bin/sh

if [ -z $1 ]; then
  echo "You must pass the name of the git repos when using this script";
  echo ""
  echo "Usage: "
  echo " $0 <nameofrepos>"
  echo " Example:"
  echo "  $0 test_app"
  echo ""
  exit 1;
fi

APP_NAME=$1
PREFIX=/opt/beehive
# Mounts
MOUNT_BASE=$PREFIX/mnt
REPOS_BASE=$PREFIX/repos
SQUASH_BASE=$PREFIX/squashed_fs
SRC_BASE=$PREFIX/src
TMP_DIR=$PREFIX/tmp
FS_DIRECTORY=$SQUASH_BASE/$APP_NAME

ERL=$(which erl)

echo "-----> Loading up $APP_NAME"
# JUST FOR DEMO-SAKES
sudo /bin/bash $PREFIX/bin/create_dyno.sh create $APP_NAME
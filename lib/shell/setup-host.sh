#!/bin/sh

USER=$1

if [ -z $USER ]; then
  USER="beehive"
fi

PREFIX=/opt/beehive

# Required directories
MOUNT_BASE=$PREFIX/mnt
REPOS_BASE=$PREFIX/repos
SQUASH_BASE=$PREFIX/squashed_fs
SRC_BASE=$PREFIX/src
TMP_DIR=$PREFIX/tmp
LOG_DIR=$PREFIX/logs

required_packages="build-essential curl git-core squashfs-tools erlang-nox"
web_packages="ruby1.8 ruby1.8-dev rubygems openssl nginx"

echo 'Setting up base at $PREFIX...'
if [ -e $LOOP_DEVICE ]; then
  echo "OK";
else
  echo 'Base does not exist! Creating it...';
  # Make the base directories
  if [ ! -d $PREFIX ]; then
    sudo mkdir -p $PREFIX
    sudo chown -R $USER $PREFIX
  fi
  if [ ! -d $MOUNT_BASE ]; then mkdir -p $MOUNT_BASE; fi
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
  if [ ! -d $LOG_DIR ]; then
    mkdir -p $LOG_DIR
  fi
fi

echo "Installing required packages";
sudo apt-get update;
sudo apt-get install -y $required_packages;
sudo apt-get install -y $web_packages;
echo "OK"

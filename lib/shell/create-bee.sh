#!/bin/sh

progdir=$(dirname $0)

BASE_DIR=[[WORKING_DIRECTORY]]
GIT_REPOS=[[LOCAL_GIT_REPOS]]
SQUASHED_DIR=[[SQUASHED_DIRECTORY]]

if [ ! -d [[WORKING_DIRECTORY]] ]; then
	mkdir -p [[WORKING_DIRECTORY]]
fi
cd [[WORKING_DIRECTORY]]

APP_NAME=[[APP_NAME]]
OUTFILE=$SQUASHED_DIR/$APP_NAME/$APP_NAME.squashfs

APP_DIR=[[WORKING_DIRECTORY]]/$APP_NAME	
GIT_REPOS_DIR=$APP_DIR/home/app

echo "app_dir $APP_DIR"
rm -rf $APP_DIR

# Here we are creating the chroot jail
APP_NAME=[[APP_NAME]]
BASE_DIR=[[WORKING_DIRECTORY]]

if [ -z "$BASE_DIR" ]; then
  BASE_DIR="/opt/beehive/bees"
fi

SKEL_DIR="$BASE_DIR/../base_skel"

mkdir -p $APP_DIR

# If we have a skeleton directory, then we'll use this as a base, rather
# than (re)creating it every time
if [ ! -d "$SKEL_DIR" ]; then
  # Make a base chroot directory
  mkdir -p $SKEL_DIR
  mkdir -p $SKEL_DIR/home
  mkdir -p $SKEL_DIR/etc
  mkdir -p $SKEL_DIR/bin
  mkdir -p $SKEL_DIR/lib
  mkdir -p $SKEL_DIR/usr
  mkdir -p $SKEL_DIR/dev
  mkdir -p $SKEL_DIR/var
  mkdir -p $SKEL_DIR/tmp
  mkdir -p $SKEL_DIR/proc

  cd $SKEL_DIR

  if [ -d /lib64 ]; then
    mkdir -p lib64/ >/dev/null 2>&1
  fi
  if [ ! -e dev/null ]; then
    mknod dev/null c 1 3 >/dev/null 2>&1
  fi
  if [ ! -e dev/zero ]; then
    mknod dev/zero c 1 5 >/dev/null 2>&1
  fi

  chmod 666 dev/null
  chmod 666 dev/zero
  
  touch etc/passwd
  grep /etc/passwd -e "^root" > etc/passwd
  
  grep /etc/group -e "^root" -e "^users" > etc/group
  
  mkdir -p $APP_DIR
fi

if [ ! -d $APP_DIR/home ]; then
  cp -R $SKEL_DIR/* $APP_DIR/
fi

if [ $(cat /etc/passwd | grep $APP_NAME | grep -v "#" | wc -l) -eq 0 ]; then
  useradd -m -p "testlogin" -s /bin/bash -d $APP_DIR/./home -c "$APP_NAME user" -g users $APP_NAME;
  grep /etc/passwd -e "^$APP_NAME" >> $APP_DIR/etc/passwd
	echo "added_user $APP_NAME"
else
  echo "user_exists $APP_NAME";
fi

echo "done $APP_DIR"

# 

# Yes, this is redundant, but... will do for now
git clone --depth 0 [[GIT_REPOS]] $GIT_REPOS_DIR >/dev/null 2>&1
cd $GIT_REPOS_DIR
SHA=$(git log --max-count=1 | awk '/commit/ {print $2}')
echo "sha $SHA"
cd ../
echo "cloned $GIT_REPOS_DIR"

# Clean the git repos of the pesky .git directory... 'cause who needs it anyway
rm -rf $GIT_REPOS_DIR/.git

# Install from files
GEM_FILE=$GIT_REPOS_DIR/.gems
if [ -f $GEM_FILE ]; then
  for line in $(cat $1/.gems); do
    gem install $line --no-ri --no-rdoc;
  done
fi
echo "installed gems"

# Make sure this directory doesn't exist
if [ -f $OUTFILE ]; then
	mv $OUTFILE $OUTFILE.old
fi

# Make the squashfs filesystem
T_DIR=$(dirname $OUTFILE)
if [ ! -d $T_DIR ]; then
	mkdir -p $T_DIR
fi

mksquashfs $APP_DIR $OUTFILE >/dev/null 2>&1

if [ $? != 0 ]; then
	exit 1
else
	echo "created_bee true"
fi
# mksquashfs $TMP_GIT_CLONE $FS_DIRECTORY/$TIMESTAMPED_NAME >/dev/null 2>&1
# # Link it as the "latest" filesystem
# ln -sf $FS_DIRECTORY/$TIMESTAMPED_NAME $MOUNT_FILE

dir_size=`du -h -s $APP_DIR | awk '{print $1}'`
bee_size=`du -h -s $OUTFILE | awk '{print $1}'`
echo "bee_size $bee_size"
echo "dir_size $dir_size"
echo "outdir $OUTFILE"

# Cleanup
rm -rf $APP_DIR
echo "finished true"
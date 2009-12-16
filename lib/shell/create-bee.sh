#!/bin/sh

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
OUT=$(create-chroot.sh $APP_NAME [[WORKING_DIRECTORY]])

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
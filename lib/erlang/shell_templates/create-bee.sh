#!/bin/sh

progdir=$(dirname $0)

GIT_REPOS=[[GIT_REPOS]]
SQUASHED_DIR=[[SQUASHED_DIRECTORY]]
# Here we are creating the chroot jail
APP_NAME=[[APP_NAME]]
BASE_DIR=[[SQUASHED_DIRECTORY]]
WORKING_DIRECTORY=[[WORKING_DIRECTORY]]

# MESSAGES
COULD_NOT_PULL_BEE=1

if [ ! -d $WORKING_DIRECTORY ]; then
	mkdir -p $WORKING_DIRECTORY
fi
cd $WORKING_DIRECTORY

APP_DIR=$WORKING_DIRECTORY/$APP_NAME
GIT_REPOS_DIR=$APP_DIR/app
FILESYSTEM=ext3

echo "app_dir $APP_DIR"
rm -rf $APP_DIR

echo "done $APP_DIR"

# Yes, this is redundant, but... will do for now
git clone --depth 0 $GIT_REPOS $GIT_REPOS_DIR
if [ $? != 0 ]; then exit $COULD_NOT_PULL_BEE; fi
  
cd $GIT_REPOS_DIR
SHA=$(git log --max-count=1 | awk '/commit/ {print $2}')
echo "sha $SHA"
cd ../
echo "cloned $GIT_REPOS_DIR"

# Clean the git repos of the pesky .git directory... 'cause who needs it anyway
rm -rf $GIT_REPOS_DIR/.git

# Install from files
GEM_FILE=$GIT_REPOS_DIR/.gems

mkdir -p $APP_DIR/.gems
export GEM_HOME="$APP_DIR/.gems"
export GEM_PATH="$APP_DIR/.gems"
if [ -f $GEM_FILE ]; then
  for line in $(cat $GIT_REPOS_DIR/.gems); do
    echo "installing $line"
    gem install $line --no-ri --no-rdoc;
  done
fi
echo "installed gems"

OUTFILE=$SQUASHED_DIR/$APP_NAME/$APP_NAME.$SHA.img
LOOPDIR=$SQUASHED_DIR/$APP_NAME/$APP_NAME/$SHA
mkdir -p $LOOPDIR

# Make sure this directory doesn't exist
if [ -f $OUTFILE ]; then
	mv $OUTFILE $OUTFILE.old
fi

# Make the squashfs filesystem
T_DIR=$(dirname $OUTFILE)
if [ ! -d $T_DIR ]; then
	mkdir -p $T_DIR
fi

dir_size=`du -s $APP_DIR | awk '{print $1+1024}'`
# Make the file
echo "dd if=/dev/zero of=$OUTFILE count=$dir_size bs=1024" > /tmp/app.log
dd if=/dev/zero of=$OUTFILE count=$dir_size bs=1K
mkfs.$FILESYSTEM -F $OUTFILE
echo "mounting: mount -o loop -t $FILESYSTEM $OUTFILE $LOOPDIR" >> /tmp/app.log
mount -o loop -t $FILESYSTEM $OUTFILE $LOOPDIR
rsync -a $APP_DIR/ $LOOPDIR
echo "unmounting: umount $LOOPDIR" >> /tmp/app.log
umount $LOOPDIR

# mksquashfs $APP_DIR $OUTFILE >/dev/null 2>&1

if [ $? != 0 ]; then
	exit 1
else
	echo "created_bee true"
fi
# mksquashfs $TMP_GIT_CLONE $FS_DIRECTORY/$TIMESTAMPED_NAME >/dev/null 2>&1
# # Link it as the "latest" filesystem
# ln -sf $FS_DIRECTORY/$TIMESTAMPED_NAME $MOUNT_FILE

bee_size=`du -h -s $OUTFILE | awk '{print $1}'`
echo "bee_size $bee_size"
echo "dir_size $dir_size"
echo "outdir $OUTFILE"

# Cleanup
rm -rf $APP_DIR
rm -rf $LOOPDIR
echo "finished true"
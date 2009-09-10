#!/bin/sh

if [ -z $1 ]; then
  echo "You must pass the name of the git repos when using this script";
  echo ""
  echo "Usage: "
  echo " $0 <nameofrepos>"
  echo " Example:"
  echo "  git-bare-setup.sh test_app"
  echo ""
  echo "  This will create a post commit that mounts the latest directory in the mount location"
  echo "  under the storage directory in squashfs"
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

cd `dirname $APP_NAME`

if [ ! -d $REPOS_BASE/$APP_NAME ]; then
  echo "mkdir $REPOS_BASE/$APP_NAME"
  mkdir -p $REPOS_BASE/$APP_NAME
fi
if [ ! -d $REPOS_BASE/$APP_NAME.git ]; then
  echo "cd $REPOS_BASE/$APP_NAME && git --bare init"
  cd $REPOS_BASE/$APP_NAME && git --bare init
fi

echo "Making post-commit hook"
STR="#!/bin/sh

# Bash script from beehive (http://github.com/auser/beehive.git)
# Runs after a commit and stores sqsh files in $STORE_DIRECTORY
# Make sure this is in your /etc/sudoers file
# %admin ALL=(ALL) NOPASSWD: /bin/mount,/bin/umount

DATE=\$(date +%s)
TIMESTAMPED_NAME=$APP_NAME-\$DATE.sqsh
GIT_REPOS=\$(pwd)
TMP_GIT_CLONE=$TMP_DIR/$APP_NAME
FS_DIRECTORY=$SQUASH_BASE/$APP_NAME

MOUNT_FILE=$FS_DIRECTORY/$APP_NAME.sqsh
MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME

if [ ! -d $MOUNT_LOCATION ]; then
  mkdir -p $MOUNT_LOCATION
fi

if [ ! -d $FS_DIRECTORY ]; then
  mkdir -p $FS_DIRECTORY
fi

# Checkout the git repos
git clone \$GIT_REPOS \$TMP_GIT_CLONE

# Make the squashfs filesystem
mksquashfs \$TMP_GIT_CLONE $FS_DIRECTORY/\$TIMESTAMPED_NAME

# Link it
ln -sf $FS_DIRECTORY/\$TIMESTAMPED_NAME \$MOUNT_FILE

# Get it ready to mount

# Make sure it's in the fstab so it will auto mount on reboot
if [ \$( grep \"$APP_NAME\" /etc/fstab | wc -l) -eq 0 ]; then
  echo \"\$MOUNT_FILE $MOUNT_LOCATION squashfs  ro,users,auto,nohide 0 0\" >> /etc/fstab
fi

# Unmount the old one
if [ ! \$(mount | grep -q $APP_NAME) ]; then
  for i in \$(mount | grep $APP_NAME | awk '{print \$1}'); do
    echo 'unmounting \$i'
    sudo umount \$i
  done
fi

# Mount the new one!
sudo mount \$MOUNT_FILE $MOUNT_LOCATION -t squashfs -o loop

# Clean up
rm -Rf \$TMP_GIT_CLONE
"

echo "$STR" > hooks/post-receive
chmod +x hooks/post-receive

STR="
  The repos is all setup.
  
  In your local repos, run:
    git remote add origin ssh://SERVER_NAME$REPOS_BASE/$APP_NAME
  
  Where the SERVER_NAME is the name of this server
"
echo "$STR"
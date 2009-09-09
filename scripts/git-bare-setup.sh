#!/bin/sh

if [[ -z $1 ]]; then
  echo "You must pass the name of the git repos when using this script";
  echo ""
  echo "Usage: "
  echo " $0 <nameofrepos> <storage directory> <mount location>"
  echo " Example:"
  echo "  git-bare-setup.sh test_app /home/auser/repos /mnt"
  echo ""
  echo "  This will create a post commit that mounts the latest directory in the mount location"
  echo "  under the storage directory in squashfs"
  echo ""
  exit 1;
fi
APP_NAME=$1
STORE_DIRECTORY=$2
MOUNT_LOCATION=$3
FS_DIRECTORY=$STORE_DIRECTORY/$APP_NAME

if [[ -z $STORE_DIRECTORY ]]; then
  STORE_DIRECTORY="/var/www"
fi

if [[ -z $MOUNT_LOCATION ]]; then
  MOUNT_LOCATION="/mnt"
fi

cd `dirname $APP_NAME`

echo "mkdir $APP_NAME"
mkdir $APP_NAME

echo "cd $APP_NAME && git --bare init"
cd $APP_NAME && git --bare init

echo "Making post-commit hook"
STR="#!/bin/sh

# Bash script from beehive (http://github.com/auser/beehive.git)
# Runs after a commit and stores sqsh files in $STORE_DIRECTORY

TIMESTAMPED_NAME=$APP_NAME-\$(date +%s).sqsh
if [[ ! -d \"$FS_DIRECTORY\" ]]; then
  mkdir -p $FS_DIRECTORY
fi

# Make the squashfs filesystem
sudo mksquashfs \$GIT_DIR $FS_DIRECTORY/\$TIMESTAMPED_NAME

if [[ ! -d \"$MOUNT_LOCATION/$APP_NAME\" ]]; then
  sudo mkdir -p $MOUNT_LOCATION/$APP_NAME
fi

# Unmount the old one
if [[ ! -d \"$MOUNT_LOCATION/$APP_NAME\" ]]; then
  sudo umount $MOUNT_LOCATION/$APP_NAME
fi

# Mount the new one!
sudo mount $FS_DIRECTORY/\$TIMESTAMPED_NAME $MOUNT_LOCATION/$APP_NAME -t squashfs -o loop
"

echo "$STR" > hooks/post-commit
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
FS_DIRECTORY=$SQUASH_BASE/$APP_NAME

MOUNT_FILE=$FS_DIRECTORY/$APP_NAME.sqsh
MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME

cd `dirname $APP_NAME`

if [ ! -d $REPOS_BASE/$APP_NAME ]; then
  echo "mkdir $REPOS_BASE/$APP_NAME"
  mkdir -p $REPOS_BASE/$APP_NAME
fi
if [ ! -d $REPOS_BASE/$APP_NAME.git ]; then
  echo "cd $REPOS_BASE/$APP_NAME && git --bare init"
  cd $REPOS_BASE/$APP_NAME && git --bare init
fi
if [ ! -d $MOUNT_LOCATION ]; then
  mkdir -p $MOUNT_LOCATION
fi
if [ ! -d $FS_DIRECTORY ]; then
  mkdir -p $FS_DIRECTORY
fi


echo "Making post-commit hook"
STR="#!/bin/sh

# Bash script from beehive (http://github.com/auser/beehive.git)
# Runs after a commit and stores sqsh files in $STORE_DIRECTORY

echo \"-----> Beehive receiving push\"
sudo /bin/bash $PREFIX/bin/create_dyno.sh create $APP_NAME
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
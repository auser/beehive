#!/bin/sh

BASE_DIR=[[WORKING_DIRECTORY]]

function create_chroot_env {
  CHROOT_DIR=$1
  
  mkdir -p $CHROOT_DIR
  mkdir -p $CHROOT_DIR/{home,etc,bin,lib,usr,usr/bin,dev,tmp,proc}
  
  cd $CHROOT_DIR
  
  if [ -d /lib64 ]; then
    mkdir -p lib64/ >/dev/null 2>&1
  fi
  if [ ! -e dev/null ]; then
    mknod dev/null c 1 3 >/dev/null 2>&1
  fi
  if [ ! -e dev/zero ]; then
    mknod dev/zero c 1 5 >/dev/null 2>&1
  fi
}
function install_from_files {  
  GEM_FILE=$1/.gems
  if [ -f $GEM_FILE ]; then
    for line in $(cat $1/.gems); do
      gem install $line --no-ri --no-rdoc; 
    done
  fi
}

function clean_git_repos {
	# cleanup
	rm -rf $1/.git
}

function build_from_env {
	APP_NAME=$1
	OUTFILE=$2
	
	APP_DIR=[[WORKING_DIRECTORY]]/$APP_NAME	
  GIT_REPOS=$APP_DIR/home/app
	
  create_chroot_env $APP_DIR
  
  # Install from files
	clean_git_repos $GIT_REPOS
  install_from_files $GIT_REPOS
	
  # Make the squashfs filesystem
	mksquashfs $APP_DIR $OUTFILE >/dev/null 2>&1
	
	if [ $? != 0 ]; then
		exit 1
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
}

cd [[WORKING_DIRECTORY]]
build_from_env [[APP_NAME]] [[OUTFILE]]
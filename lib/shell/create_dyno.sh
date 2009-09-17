#!/bin/bash

PREFIX=/opt/beehive
MOUNT_BASE=$PREFIX/mnt
REPOS_BASE=$PREFIX/repos
SQUASH_BASE=$PREFIX/squashed_fs
SRC_BASE=$PREFIX/src
TMP_DIR=$PREFIX/tmp

# This process creates a dyno environment (http://heroku.com/how/dyno_grid)
function create_chroot_env {
  CHROOT_DIR=$1
  echo "-----> Creating chroot environment: $CHROOT_DIR"
  
  mkdir -p $CHROOT_DIR
  mkdir -p $CHROOT_DIR/{home,etc,bin,lib,usr,usr/bin,dev,tmp,proc}
  
  cd $CHROOT_DIR
  
  if [ -d /lib64 ]; then
    mkdir -p lib64/
  fi
  if [ ! -e dev/null ]; then
    sudo mknod dev/null c 1 3
  fi
  if [ ! -e dev/zero ]; then
    sudo mknod dev/zero c 1 5
  fi
}
function install_from_files {
  BASE_DIR=$1
  
  GEM_FILE=$BASE_DIR/.gems
  if [ -f $GEM_FILE ]; then
    for line in $(cat $BASE_DIR/.gems); do
      sudo gem install $line --no-ri --no-rdoc; 
    done
  fi
}

function build_from_env {
  APP_NAME=$1
  GIT_REPOS=$REPOS_BASE/$APP_NAME
  
  FS_DIRECTORY=$SQUASH_BASE/$APP_NAME
  MOUNT_FILE=$FS_DIRECTORY/$APP_NAME.sqsh
  
  DATE=$(date +%s)
  TIMESTAMPED_NAME=$APP_NAME-$DATE.sqsh
  TMP_GIT_CLONE=$TMP_DIR/$APP_NAME
  
  echo "-----> Building application slug: $APP_NAME"
  # Make the base environment
  mkdir -p $TMP_GIT_CLONE/home
  
  cd $TMP_GIT_CLONE
  echo "-----> Checking out latest application"
  git clone $GIT_REPOS $TMP_GIT_CLONE/home/app
  
  create_chroot_env $TMP_GIT_CLONE
  
  # Install from files
  install_from_files $TMP_GIT_CLONE/home/app
    
  # Make the squashfs filesystem
  echo "-----> Creating and squashing dyno"
  mksquashfs $TMP_GIT_CLONE $FS_DIRECTORY/$TIMESTAMPED_NAME >/dev/null 2>&1
  # Link it as the "latest" filesystem
  ln -sf $FS_DIRECTORY/$TIMESTAMPED_NAME $MOUNT_FILE
  
  dir_size=`sudo du -h -s $FS_DIRECTORY/$TIMESTAMPED_NAME | awk '{print $1}'`
  dyno_size=`sudo du -h -s $MOUNT_FILE | awk '{print $1}'`
  echo "-----> Dyno is $dyno_size (from $dir_size)"
  mount_and_bind $APP_NAME
  
  # Cleanup
  rm -rf $TMP_GIT_CLONE
}

function run_apps {
  CHROOT_USER=$(get_user_id_from_app_name $APP_NAME)
  PORT=$(get_port_from_user_id $APP_NAME)
  if [ -f home/app/config.ru ]; then
    echo "-----> config.ru found. Running thin"
    GEM_BIN_DIR=$(gem env | grep EXECUTABLE | grep DIRECTORY | awk '{print $4}')
    sudo -u $CHROOT_USER /usr/bin/env -i \
                      HOME=./home/app \
                      $GEM_BIN_DIR/thin -s 1 \
                      -R home/app/config.ru \
                      -p $PORT \
                      -P tmp/pids/thin.$APP_NAME.pid \
                      -d \
                      -l tmp/thin.$APP_NAME.log \
                      start
 fi
}
function mount_and_bind {
  APP_NAME=$1
  MOUNT_LOCATION=$MOUNT_BASE/$APP_NAME
  LOOP_DEVICE=/dev/$APP_NAME
  
  if [ ! -e $LOOP_DEVICE ]; then
    sudo mknod -m 600 $LOOP_DEVICE b 7 0
  fi
  
  # Unmount the already mounted app files
  unmount_already_mounted $MOUNT_LOCATION $APP_NAME $LOOP_DEVICE
  
  echo "-----> sudo mount $MOUNT_FILE $MOUNT_LOCATION -t squashfs -o ro -o loop=$LOOP_DEVICE"
	# Mount the loop'd filesystem
  sudo mount $MOUNT_FILE $MOUNT_LOCATION -t squashfs -o ro -o loop=$LOOP_DEVICE
  
  # Bind mount the system
  sudo mount --bind /bin $MOUNT_LOCATION/bin -o ro
  sudo mount --bind /etc $MOUNT_LOCATION/etc -o ro
  sudo mount --bind /usr $MOUNT_LOCATION/usr -o ro
  sudo mount --bind /lib $MOUNT_LOCATION/lib -o ro
  sudo mount --bind /dev $MOUNT_LOCATION/dev -o ro
  sudo mount -t proc /proc $MOUNT_LOCATION/proc
  sudo mount --bind /tmp $MOUNT_LOCATION/tmp -o rw
  
  # sudo mount -t unionfs -o dirs=/home none $MOUNT_LOCATION/home
  
  # If there is a lib64 directory 
  if [ -d /lib64 ]; then
    sudo mount --bind /lib64 $MOUNT_LOCATION/lib64 -o ro
  fi
  
  echo "-----> Chrooting into $MOUNT_LOCATION"
  cd $MOUNT_LOCATION
  
  # Create chroot user
  CHROOT_USER=$(get_user_id_from_app_name $APP_NAME)
  
  sudo /usr/sbin/chroot $MOUNT_LOCATION /usr/bin/env -i \
           HOME=/home/app 
           TERM=$TERM PS1='\u:\w\$ ' \
           HI="hi" \
         /bin/bash --login -c "echo ''";
  
  if [ $(sudo cat /etc/passwd | grep $CHROOT_USER | grep -v "#" | wc -l) -eq 0 ]; then
    useradd -s /bin/bash -d $MOUNT_LOCATION/./ -c "$APP_NAME user" -g users $CHROOT_USER;
  else
    echo "";
  fi
  
  echo "-----> Running apps"
  run_apps $CHROOT_USER
}

function unmount_already_mounted {
  MOUNT_LOCATION=$1
  APP_NAME=$2
  LOOP_DEVICE=$3
  
  # First stop the thin app if it's already running
  PORT=$(get_port_from_user_id $APP_NAME)
  
  if [ -f $MOUNT_LOCATION/tmp/pids/thin.$APP_NAME.$PORT.pid ]; then
		echo "-----> Killing current running thin app"
    GEM_BIN_DIR=$(gem env | grep EXECUTABLE | grep DIRECTORY | awk '{print $4}')
    $GEM_BIN_DIR/thin -s 1 \
                      -R home/app/config.ru \
                      -p $PORT \
                      -P $MOUNT_LOCATION/tmp/pids/thin.$APP_NAME.pid \
                      --prefix $MOUNT_LOCATION \
                      -d \
                      -c $MOUNT_LOCATION \
                      -l $MOUNT_LOCATION/tmp/thin.$APP_NAME.log \
                      stop

  fi
  
  MOUNTED=$(mount | grep $MOUNT_LOCATION | awk '{a[i++]=$3} END {for (j=i-1; j>=0;) print a[j--] }')
  for i in $MOUNTED; do
    sudo umount $i -f >/dev/null 2>&1
  done
  sudo umount $LOOP_DEVICE >/dev/null 2>&1
}

function get_user_id_from_app_name {
  APP_NAME=$1;
  echo "$APP_NAME""_user"
}
function get_port_from_user_id {
  APP_NAME=$1
  CHROOT_USER=$(get_user_id_from_app_name $APP_NAME)
  USER_ID=$(sudo cat /etc/passwd | grep $CHROOT_USER | sed -e 's/:/ /g' | awk '{print $3}')
  echo $(($USER_ID+4000))
}

function show_usage {
  echo ""
  echo "Usage: $0 (create|destroy) <name>"
  echo ""
}

if [ -z $2 ]; then
  show_usage
  exit 1;
fi
case $1 in
  create )
      echo "-----> Creating new dyno: $2"
      build_from_env $2;
    ;;
  destroy )
      echo "-----> Destroying dyno $2"
      # unmount_already_mounted $MOUNT_BASE/$2
    ;;
  * )
    show_usage
    exit 1
esac

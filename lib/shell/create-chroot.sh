#!/bin/bash

if [ $# -lt 1 ]; then
  echo "Usage: $0 app_dir skel_dir [optional]"
  exit 1
fi

APP_NAME=[[APP_NAME]]
BASE_DIR=[[BASE_DIR]]

APPS="/bin/sh /bin/bash /bin/cp /bin/ls /bin/mkdir /bin/mv /usr/bin/env"
APPS="$APPS /bin/pwd /bin/rm /bin/rmdir /usr/bin/id /usr/bin/ssh /bin/ping /usr/bin/vi"

if [ -z "$BASE_DIR" ]; then
  BASE_DIR="/opt/beehive/bees"
fi

SKEL_DIR="$BASE_DIR/../base_skel"

APP_DIR="$BASE_DIR/$APP_NAME"
mkdir -p $APP_DIR

# If we have a skeleton directory, then we'll use this as a base, rather
# than (re)creating it every time
if [ ! -d "$SKEL_DIR" ]; then
  # Make a base chroot directory
  mkdir -p $SKEL_DIR
  mkdir -p $SKEL_DIR/home
  mkdir -p $SKEL_DIR/etc/pam.d/
  mkdir -p $SKEL_DIR/bin
  mkdir -p $SKEL_DIR/lib
  mkdir -p $SKEL_DIR/usr
  mkdir -p $SKEL_DIR/usr/lib/openssh
  mkdir -p $SKEL_DIR/usr/bin
  mkdir -p $SKEL_DIR/dev
  mkdir -p $SKEL_DIR/var
  mkdir -p $SKEL_DIR/tmp

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
  
  cp /lib/libnss_compat.so.2 /lib/libnsl.so.1 /lib/libnss_files.so.2 /lib/ld-linux.so.2 /lib/libnss_dns.so.2 ./lib/
  
  PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

  for prog in $APPS;  do
    mkdir -p ./`dirname $prog` > /dev/null 2>&1
    cp $prog ./$prog
    # obtain a list of related libraries
    ldd $prog > /dev/null
    if [ "$?" = 0 ] ; then
      LIBS=`ldd $prog | awk '{ print $3 }'`
      for l in $LIBS; do
        mkdir -p ./`dirname $l` > /dev/null 2>&1
        cp $l ./$l  > /dev/null 2>&1
      done
    fi
  done
  
  cp /etc/hosts etc/
  cp /etc/resolv.conf etc/
  cp /etc/pam.d/* etc/pam.d/
  cp -r /lib/security lib/
  cp -r /etc/security etc/
  cp /etc/login.defs etc/
  cp /usr/lib/libgssapi_krb5.so.2 usr/lib/
  cp /usr/lib/libkrb5.so.3 usr/lib/
  cp /usr/lib/libk5crypto.so.3 usr/lib/
  cp /lib/libcom_err.so.2 lib/
  cp /usr/lib/libkrb5support.so.0 usr/lib/
  
  echo '#!/bin/bash' > usr/bin/groups
  echo "id -Gn" >> usr/bin/groups
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
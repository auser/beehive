#!/bin/bash -e
BEEHIVE_USER_HOME=${1:-'/var/lib/beehive'}
HOME=$BEEHIVE_USER_HOME
INSTALL_PREFIX=${2:-''}

sudo apt-get update
sudo apt-get install -y curl git-core
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev erlang-tools

## Prepare beehive directories
sudo mkdir -p $BEEHIVE_USER_HOME
if [ $(sudo cat /etc/passwd | grep ^beehive | grep -v "#" | wc -l) -eq 0 ]; then
  useradd -s /bin/bash -b $BEEHIVE_USER_HOME -d $BEEHIVE_USER_HOME -c "beehive user" -g users beehive;
fi
echo "HwlloE0lrd" > $BEEHIVE_USER_HOME/.erlang.cookie
sudo chmod 600 $BEEHIVE_USER_HOME/.erlang.cookie
sudo chown beehive -R $BEEHIVE_USER_HOME

cd /tmp
git clone --depth 0 git://github.com/auser/beehive.git
cd beehive/lib/erlang
make deps
make
make boot
sudo make install

echo " starting beehive.. "
$INSTALL_PREFIX/usr/bin/start_beehive -d -t router
echo " -- completed router user-data script ---"

sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y git git-core
sudo apt-get install -y rubygems1.8
sudo apt-get install -y squashfs-tools
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev
sudo apt-get install -y python-setuptools

## Prepare beehive directories
sudo mkdir -p $BEEHIVE_USER_HOME
if [ $(sudo cat /etc/passwd | grep ^beehive | grep -v "#" | wc -l) -eq 0 ]; then
  useradd -s /bin/bash -b $BEEHIVE_USER_HOME -d $BEEHIVE_USER_HOME -c "beehive user" -g users beehive;
fi
sudo cp ~/.bashrc ~beehive/.bashrc
echo "HwlloE0lrd" > $BEEHIVE_USER_HOME/.erlang.cookie
sudo chmod 600 $BEEHIVE_USER_HOME/.erlang.cookie
sudo chown beehive -R $BEEHIVE_USER_HOME

####### behive stuff
# mkdir -p $BEEHIVE_HOME/src && cd $BEEHIVE_HOME/src
# git clone git@github.com:auser/beehive.git
# curl -o $BEEHIVE_HOME/src/beehive.tgz https://github.com/auser/beehive/tarball/master
cd $SRC_DIR/lib/erlang
make deps
make
make boot
sudo make install
chown -R beehive $INSTALL_PREFIX
cd $SRC_DIR

# Install gitosis
cd /tmp
git clone git://eagain.net/gitosis.git 
cd gitosis

sudo python setup.py install

if [ $(cat /etc/passwd | grep git | grep -v "#" | wc -l) -eq 0 ]; then
  sudo adduser \
      --system \
      --shell /bin/sh \
      --gecos 'git version control' \
      --group \
      --disabled-password \
      --home /home/git \
      git
  sudo -H -u git mkdir /home/git/.ssh

  if [ -f /tmp/id_rsa.pub ]; then
    sudo -H -u git gitosis-init < /tmp/id_rsa.pub
    mv /tmp/id_rsa.pub /home/git/.ssh/id_rsa.pub
  else
    ssh-keygen -f ~/.ssh/id_rsa -N "hello"
    sudo -H -u git gitosis-init < ~/.ssh/id_rsa.pub
  fi
else
  echo "git_user exists";
fi

sudo chmod 755 /home/git/repositories/gitosis-admin.git/hooks/post-update

if [ ! -f /etc/init.d/git-daemon ]; then
  echo "#! /bin/sh

  PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
  NAME=git-daemon
  PIDFILE=/var/run/\$NAME.pid
  DESC=\"the git daemon\"
  DAEMON=\"/usr/bin/git daemon\"
  DAEMON_OPTS=\"--base-path=/home/git/repositories --verbose --syslog
              --detach --pid-file=\$PIDFILE --user=git --group=nogroup\"

  [ -r /etc/default/git-daemon ] && . /etc/default/git-daemon

  . /lib/lsb/init-functions

  start_git() {
    start-stop-daemon --start --quiet --pidfile \$PIDFILE \
      --startas \$DAEMON -- \$DAEMON_OPTS
  }

  stop_git() {
    start-stop-daemon --stop --quiet --pidfile \$PIDFILE
    rm -f \$PIDFILE
  }

  status_git() {
    start-stop-daemon --stop --test --quiet --pidfile \$PIDFILE >/dev/null 2>&1
  }

  case \"\$1\" in
    start)
    log_begin_msg \"Starting \$DESC\"
    start_git
    log_end_msg 0
    ;;
    stop)
    log_begin_msg \"Stopping \$DESC\"
    stop_git
    log_end_msg 0
    ;;
    status)
    log_begin_msg \"Testing \$DESC: \"
    if status_git
    then
      log_success_msg \"Running\"
      exit 0
    else
      log_failure_msg \"Not running\"
      exit 1
    fi
    ;;
    restart|force-reload)
    log_begin_msg \"Restarting \$DESC\"
    stop_git
    sleep 1
    start_git
    log_end_msg 0
    ;;
    *)
    echo \"Usage: \$0 {start|stop|restart|force-reload|status}\" >&2
    exit 1
    ;;
  esac

  exit 0" > /etc/init.d/git-daemon

  sudo chmod +x /etc/init.d/git-daemon
  sudo invoke-rc.d git-daemon start
else
  echo "git daemon already exists"
fi

rm -rf /tmp/gitosis

echo " -- completed storage user-data script ---"


sudo apt-get install -y build-essential libc6-dev m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev libopenssl-ruby
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev
sudo apt-get install -y squashfs-tools

# So we can deploy thin and rack apps
sudo gem install rack thin --no-rdoc --no-ri
sudo gem install haml sinatra --no-rdoc --no-ri

# Create as many loop back devices as we can
for i in $(seq 0 255); do
	sudo mknod -m0660 /dev/loop$i b 7 $i >/dev/null 2>&1
done

## Prepare beehive directories
sudo mkdir -p $BEEHIVE_USER_HOME
if [ $(sudo cat /etc/passwd | grep ^beehive | grep -v "#" | wc -l) -eq 0 ]; then
  useradd -s /bin/bash -b $BEEHIVE_USER_HOME -d $BEEHIVE_USER_HOME -c "beehive user" -g users beehive;
fi
sudo cp ~/.bashrc ~beehive/.bashrc
echo "HwlloE0lrd" > $BEEHIVE_USER_HOME/.erlang.cookie
sudo chmod 600 $BEEHIVE_USER_HOME/.erlang.cookie
sudo chown beehive -R $BEEHIVE_USER_HOME

####### behive stuff
# mkdir -p $BEEHIVE_HOME/src && cd $BEEHIVE_HOME/src
# git clone git@github.com:auser/beehive.git
# curl -o $BEEHIVE_HOME/src/beehive.tgz https://github.com/auser/beehive/tarball/master
cd $SRC_DIR/lib/erlang
make deps
make
make boot
sudo make install
chown -R beehive $INSTALL_PREFIX
cd $SRC_DIR

echo " -- completed node user-data script ---"
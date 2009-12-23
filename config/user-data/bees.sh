#!/bin/bash

BEEHIVE_USER_HOME=${1:-'/var/lib/beehive'}
INSTALL_PREFIX=${2:-''}
SRC_DIR="/tmp/beehive"

sudo apt-get update -y
sudo apt-get install -y curl git-core
sudo apt-get install -y build-essential libc6-dev m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev libopenssl-ruby
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev

# So we can deploy thin and rack apps
sudo gem install rack thin --no-rdoc --no-ri
sudo gem install haml sinatra --no-rdoc --no-ri

## Prepare beehive directories
sudo mkdir -p $BEEHIVE_USER_HOME
if [ $(sudo cat /etc/passwd | grep ^beehive | grep -v "#" | wc -l) -eq 0 ]; then
  sudo useradd -s /bin/bash -b $BEEHIVE_USER_HOME -d $BEEHIVE_USER_HOME -c "beehive user" -g users beehive;
fi

FIRST_N_CHARS_OF_PUB_KEY=`curl http://169.254.169.254/latest/meta-data/public-keys/0/openssh-key/ | awk '{str=$2} END {print substr(str, 0, 30)}'`
echo $FIRST_N_CHARS_OF_PUB_KEY > /tmp/.erlang.cookie
sudo mv /tmp/.erlang.cookie $BEEHIVE_USER_HOME
sudo chmod 600 $BEEHIVE_USER_HOME/.erlang.cookie
sudo chown beehive -R $BEEHIVE_USER_HOME
sudo cp $BEEHIVE_USER_HOME/.erlang.cookie /root/.erlang.cookie

####### behive stuff
# mkdir -p $BEEHIVE_HOME/src && cd $BEEHIVE_HOME/src
git clone --depth 0 git://github.com/auser/beehive.git $SRC_DIR
# curl -o $BEEHIVE_HOME/src/beehive.tgz https://github.com/auser/beehive/tarball/master
cd $SRC_DIR/lib/erlang
sudo make
sudo make install
cd $SRC_DIR

# Root has to run stuff... hrmph!
sudo cp -R $BEEHIVE_USER_HOME/* /root

# Start the beehive
sudo -H -u beehive $INSTALL_PREFIX/usr/bin/start_beehive -d -t node -s 'router@domU-12-31-38-04-C4-68.compute-1.internal'
# Root needs to mount - TODO
sudo -H -u root $INSTALL_PREFIX/usr/bin/start_beehive -d -t storage -s 'router@domU-12-31-38-04-C4-68.compute-1.internal'

# Create as many loop back devices as we can
for i in $(seq 0 255); do
  sudo mknod -m0660 /dev/loop$i b 7 $i
done

echo " -- completed bee user-data script ---"

#!/bin/bash -e

BEEHIVE_USER_HOME=${1:-'/var/lib/beehive'}
INSTALL_PREFIX=${2:-''}
SRC_DIR="/tmp/beehive"

sudo apt-get update -y
sudo apt-get install -y curl
sudo apt-get install -y build-essential libc6-dev m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev libopenssl-ruby
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev erlang-tools
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
  sudo useradd -s /bin/bash -b $BEEHIVE_USER_HOME -d $BEEHIVE_USER_HOME -c "beehive user" -g users beehive;
fi

sudo touch $BEEHIVE_USER_HOME/.erlang.cookie

echo "HwlloE0lrd" > /tmp/.erlang.cookie
sudo mv /tmp/.erlang.cookie $BEEHIVE_USER_HOME
sudo chmod 600 $BEEHIVE_USER_HOME/.erlang.cookie
sudo chown beehive -R $BEEHIVE_USER_HOME

####### behive stuff
# mkdir -p $BEEHIVE_HOME/src && cd $BEEHIVE_HOME/src
git clone --depth 0 git@github.com:auser/beehive.git $SRC_DIR
# curl -o $BEEHIVE_HOME/src/beehive.tgz https://github.com/auser/beehive/tarball/master
cd $SRC_DIR/lib/erlang
make
sudo make install
cd $SRC_DIR

echo " -- completed node user-data script ---"
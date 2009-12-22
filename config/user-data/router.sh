#!/bin/bash -e
BEEHIVE_USER_HOME=${1:-'/var/lib/beehive'}
INSTALL_PREFIX=${2:-''}

sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y ruby rubygems ruby-dev
sudo apt-get install -y curl
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev

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
SRC_DIR="/home/ubuntu/beehive"

cd $SRC_DIR/lib/erlang
make deps
make
make boot
sudo make install
chown -R beehive $INSTALL_PREFIX
cd $SRC_DIR

echo " -- completed router user-data script ---"
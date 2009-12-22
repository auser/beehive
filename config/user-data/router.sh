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
sudo make
sudo make install

echo " starting beehive.. "
sudo $INSTALL_PREFIX/usr/bin/start_beehive -d -t router
echo " -- completed router user-data script ---"
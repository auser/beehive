#!/bin/bash -e
BEEHIVE_USER_HOME=${1:-'/var/lib/beehive'}
HOME=$BEEHIVE_USER_HOME
INSTALL_PREFIX=${2:-''}

sudo apt-get update
sudo apt-get install -y curl git-core
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev libcap-dev
sudo apt-get install -y ruby rubygems ruby-dev
sudo apt-get install -y spidermonkey-bin
sudo apt-get install -y erlang-nox erlang-base-hipe erlang-dev erlang-tools

cd /tmp

# Grab and install jsawk
curl http://github.com/micha/jsawk/raw/master/jsawk > jsawk
chmod +x jsawk
sudo mv jsawk /usr/bin

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

SRC_DIR="/tmp/beehive"
git clone --depth 0 git://github.com/auser/beehive.git $SRC_DIR
# curl -o $BEEHIVE_HOME/src/beehive.tgz https://github.com/auser/beehive/tarball/master
cd $SRC_DIR/lib/erlang
sudo make
sudo make install
cd $SRC_DIR

echo " starting beehive.. "
sudo -H -u root $INSTALL_PREFIX/usr/bin/start_beehive -d -t router -p 80
echo " -- completed router user-data script ---"

HOSTNAME=`hostname -f`
curl --basic --user getbeehive:$FIRST_N_CHARS_OF_PUB_KEY --data status="$HOSTNAME" http://twitter.com/statuses/update.xml
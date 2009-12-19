#!/bin/sh

sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y git git-core
sudo apt-get install -y rubygems1.8
sudo apt-get install -y squashfs-tools
sudo apt-get autoremove -y

mkdir -p $PREFIX/squashed

echo "HwlloE0lrd" > ~/.erlang.cookie

# Install gitosis
sudo apt-get install python-setuptools
cd /tmp
git clone git://eagain.net/gitosis.git 
cd gitosis

sudo python setup.py install

sudo adduser --system --shell /bin/bash --gecos 'git version control' --group --disabled-password --home /home/git git
sudo -H -u git mkdir /home/git/.ssh

if [ -f /tmp/id_rsa.pub ]; then
  sudo -H -u git gitosis-init < /tmp/id_rsa.pub
  rm /tmp/id_rsa.pub
else
  ssh-keygen -f ~/.ssh/id_rsa -N "hello"
  sudo -H -u git gitosis-init < ~/.ssh/id_rsa.pub
fi

sudo chmod 755 /home/git/repositories/gitosis-admin.git/hooks/post-update

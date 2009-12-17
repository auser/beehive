#!/bin/sh

sudo apt-get update
sudo apt-get install -y build-essential m4 libssl-dev libncurses5 libncurses5-dev
sudo apt-get install -y git git-core
sudo apt-get install -y squashfs-tools
sudo apt-get autoremove -y

mkdir -p $PREFIX/squashed

echo "HwlloE0lrd" > ~/.erlang.cookie
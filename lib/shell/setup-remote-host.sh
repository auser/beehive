#!/bin/sh

REMOTE_HOST=$1

USER=auser
PREFIX=/opt/beehive
SCRIPT_DIR=/opt/beehive/scripts
CURR_DIR=$(pwd)/scripts
SSH_HOST=$USER@$REMOTE_HOST
SSH_CMD="ssh -i /Users/alerner/.ssh/id_rsa -o StrictHostKeyChecking=no $SSH_HOST"

$SSH_CMD "echo 'Setting up'"
$SSH_CMD "echo '%sudo ALL=NOPASSWD: ALL' | sudo tee -a /etc/sudoers"
$SSH_CMD "sudo mkdir -p $PREFIX"
$SSH_CMD "sudo mkdir -p $SCRIPT_DIR"
$SSH_CMD "sudo chown -R $USER $PREFIX"

rsync -L -e 'ssh -i /Users/alerner/.ssh/id_rsa -o StrictHostKeyChecking=no' -va $CURR_DIR/ $SSH_HOST:$SCRIPT_DIR/

$SSH_CMD $SCRIPT_DIR/setup-host.sh
$SSH_CMD $SCRIPT_DIR/setup-nginx.sh
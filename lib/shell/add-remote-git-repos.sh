#!/bin/sh

REMOTE_HOST=$1
APP_NAME=$2
USER=$3

if [ -z $REMOTE_HOST ] || [ -z $APP_NAME ]; then
  echo ""
  echo " Usage: $0 <remote host> <app name> <user (optional)>"
  echo ""
  exit
fi

if [ -z $USER ]; then
  USER=auser
fi

PREFIX=/opt/beehive
SCRIPT_DIR=/opt/beehive/scripts
SSH_HOST=$USER@$REMOTE_HOST
SSH_CMD="ssh -i /Users/alerner/.ssh/id_rsa -o StrictHostKeyChecking=no $SSH_HOST"

$SSH_CMD $SCRIPT_DIR/git-bare-setup.sh $APP_NAME
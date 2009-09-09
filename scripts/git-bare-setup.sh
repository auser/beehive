#!/bin/sh

if [[ -z $1 ]]; then
  echo "You must pass the name of the git repos when using this script";
  echo ""
  echo "Usage: "
  echo " $0 <nameofrepos>"
  echo ""
  exit 1;
fi
APP_NAME=$1
cd `dirname $APP_NAME`
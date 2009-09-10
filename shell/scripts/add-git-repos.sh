#!/bin/sh

while [ $# -gt 0 ]
do
  case $1
  in
    -l)
      LOCATION=$2
      shift 2
    ;;

    *)
      # If the remote server is already set, then show the help menu
      if [ -z $REMOTE_SERVER ]; then
        REMOTE_SERVER=$1
      else
        STR="
        Usage: $0 <remote server> OPTIONS

          Remote server - The server where the git repos will live
          Remote location - The location of the git repository

          OPTIONS
          =======================
          -l: The location on the remote machine
      "
        echo "$STR"
        exit 1;
      fi
    ;;
  esac
done

# Defaults
if [ -z LOCATION ]; then
  LOCATION="/var/beehive"
fi
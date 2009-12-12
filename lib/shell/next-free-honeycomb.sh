#!/bin/sh

BASE_DIR=[[DESTINATION]]/[[APP_NAME]]/$(date +"%m/%d/%y/%M/%S")/[[SLOT_DIR]]
mkdir -p $BASE_DIR
echo "dir $BASE_DIR"
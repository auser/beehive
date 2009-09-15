#!/bin/bash

APP_NAME=$1

GEM_BIN_DIR=$(gem env | grep EXECUTABLE | grep DIRECTORY | awk '{print $4}')
$GEM_BIN_DIR/thin -s 1 \
                  -R home/app/config.ru \
                  -p 5000 \
                  -P tmp/pids/thin.$APP_NAME.pid \
                  -d \
                  -l tmp/thin.$APP_NAME.log \
                  start

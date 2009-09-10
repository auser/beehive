#!/bin/sh

APP_NAME=$1
PREFIX=/opt/beehive
GROUP=admin
# Mounts
MOUNT_BASE=$PREFIX/mnt
LOG_DIR=$PREFIX/logs

STR="
server {
  # Replace this port with the right one for your requirements
  listen       80;
 
  server_name  _;

  set $webroot_path $MOUNT_BASE;
  root $webroot_path/$host;
  error_page  404              http://$APP_NAME/errors/404.html;
  access_log  $LOG_DIR/$APP_NAME.access.log;
 
  location / {
    root   $webroot_path/$host/;
    index  index.html;
  }
 
  location ~ /\.ht {
    deny  all;
  }
}
"
sudo apt-get install nginx -y
sudo chmod 777 /etc/nginx/conf.d
touch /etc/nginx/conf.d/virtual_domains.conf
echo "$STR" >> /etc/nginx/conf.d/virtual_domains.conf
sudo chmod 755 /etc/nginx/conf.d
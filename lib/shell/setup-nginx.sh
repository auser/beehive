#!/bin/sh

USER=$1

if [ -z $USER ]; then
  USER="root"
fi
PREFIX=/opt/beehive
# Mounts
MOUNT_BASE=$PREFIX/mnt
LOG_DIR=$PREFIX/logs

STR="
server {
  listen          80 default;
  server_name     _;
  access_log  $LOG_DIR/default.access.log;

  server_name_in_redirect  off;

  location / {
    index index.html;
    root  /var/www/nginx-default;
  }
}
"
echo "----> Setting up Nginx: for default as $USER"
sudo apt-get install -y openssl libopenssl-ruby ruby1.8-dev rubygems nginx
sudo chmod 777 /etc/nginx/conf.d
touch /etc/nginx/conf.d/virtual_domains.conf
echo "$STR" > /etc/nginx/conf.d/virtual_domains.conf
sudo chmod 755 /etc/nginx/conf.d
sudo /etc/init.d/nginx start

sudo chown $USER /etc/nginx/sites-*
sudo gem install rack thin --no-ri --no-rdoc
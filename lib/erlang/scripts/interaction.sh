#!/bin/bash

# First, get the token
HOST=localhost:8080
TOKEN=`curl -i -XPOST -d"{\"email\":\"root@getbeehive.com\", \"password\": \"test\"}" $HOST/auth | sed -e 's/[{}]/''/g' | awk -v k="text" '{n=split($0,a,","); for (i=1; i<=n; i++) print a[i]}' | grep "token" | sed 's/:/ /1' | awk -F" " '{ print $2 }'`

# Now, we have a token for future requests
# Add a user
curl -i -XPOST -d"{\"email\":\"arilerner@mac.com\", \"password\":\"myuniquepassword\", \"level\":\"1\", \"token\":$TOKEN}" $HOST/users/new

# Let's add an application
curl -i $HOST/apps
curl -i -XPOST -d"{\"url\":\"git://github.com/auser/getbeehive.com.git\", \"token\":$TOKEN}" $HOST/apps
curl -i -XPOST -d"{\"name\":\"beehive\", \"url\":\"git://github.com/auser/getbeehive.com.git\", \"token\":$TOKEN}" $HOST/apps
curl -i $HOST/apps

# Delete the app we added
curl -i -XDELETE -d"{\"token\":\"$TOKEN\"}" $HOST/apps/beehive
#!/bin/bash

HOST=localhost:8080

# First, get the token
TOKEN=`curl -i -XPOST -d"{\"email\":\"root@getbeehive.com\", \"password\": \"test\"}" $HOST/auth | sed -e 's/[{}]/''/g' | awk -v k="text" '{n=split($0,a,","); for (i=1; i<=n; i++) print a[i]}' | grep "token" | sed 's/:/ /1' | awk -F" " '{ print $2 }'`

# Now, we have a token for future requests
# Let's add an application
curl -i $HOST/apps
curl -i -XPOST -d"{\"name\":\"demo\", \"url\":\"git://github.com/auser/getbeehive.com.git\", \"token\":$TOKEN}" $HOST/apps
curl -i $HOST/apps
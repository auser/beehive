#!/bin/bash

# First, get the token
HOST=localhost:8080
TOKEN=`curl -i -XPOST -d'{"email":"root@getbeehive.com", "password": "test"}' $HOST/auth.json | sed -e 's/[{}]/''/g' | awk -v k="text" '{n=split($0,a,","); for (i=1; i<=n; i++) print a[i]}' | grep "token" | sed 's/:/ /1' | sed 's/\]//1' | awk -F" " '{ print $2 }'`

# Now, we have a token for future requests
# Add a user
curl -i -XPOST -d"{\"email\":\"arilerner@mac.com\", \"password\":\"myuniquepassword\", \"level\":\"1\", \"token\":$TOKEN}" $HOST/users/new.json

# Let's add an application
curl -i -XPOST -d"{\"name\":\"beehive\", \"url\":\"git://github.com/auser/getbeehive.com.git\", \"token\":$TOKEN, \"template\":\"rack\"}" $HOST/apps.json

# Now another
curl -i -XPOST -d"{\"name\":\"teambox\", \"url\":\"http://github.com/micho/teambox.git\", \"token\":$TOKEN, \"template\":\"rails\"}" $HOST/apps.json

# How about one that will fail?
curl -i -XPOST -d"{\"name\":\"fake\", \"url\":\"http://github.com/auser/doesnt_exist.git\", \"token\":$TOKEN, \"template\":\"rails\"}" $HOST/apps.json

# Let's fix that
curl -i -XPUT -d"{\"name\":\"fake\", \"url\":\"http://github.com/auser/getbeehive.com.git\", \"token\":$TOKEN, \"template\":\"rails\"}" $HOST/apps/fake.json

# Check on the application
curl -i $HOST/apps.json

# Update the app
curl -i -XPOST -d"{\"name\":\"beehive\", \"token\":$TOKEN}" $HOST/apps/beehive/deploy.json

# Expand the app
curl -i -XPOST -d"{\"name\":\"beehive\", \"token\":$TOKEN}" $HOST/apps/beehive/expand.json

# Delete the app we added
curl -i -XDELETE -d"{\"token\":$TOKEN}" $HOST/apps/beehive.json
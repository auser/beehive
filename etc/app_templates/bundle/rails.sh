#!/bin/sh -e

export RAILS_ENV=production
mkdir .beehive_gem_home
export GEM_HOME=.beehive_gem_home
export GEM_PATH=.beehive_gem_home

echo "$FIXTURE_DIR/gems/rack-1.2.1.gem" > /tmp/rack.env

echo "Checking for a config.ru"
if [ ! -f "config.ru" ]; then
  exit "Invalid rack app"
fi

# If there is an isolate file, run Isolate.now!
if [ -f "Isolate" ]; then
  ruby -rubygems -e "require 'isolate'; Isolate.now!"
fi

# If bundler is used
if [ -f "Gemfile" ]; then
  bundle install
fi

# If .gems file exists
if [ -f ".gems" ]; then
  while read line; do
    if [ ! -z "$line" ]; then
      echo "Installing gem install --no-rdoc --no-ri $line"
      eval "echo \"Installing gem install --no-rdoc --no-ri $line\""
      eval "gem install --no-rdoc --no-ri $line"
    fi
  done < ".gems"
fi

if [ ! -f "start.sh" ]; then
  echo "#!/bin/sh -e
  rackup -p \$PORT
  " > start.sh

  chmod u+x start.sh
fi
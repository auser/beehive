#!/bin/bash
CURL=$(which curl)
GIT=$(which git)

esc=""

black="${esc}[30m";   red="${esc}[31m";    green="${esc}[32m"
yellow="${esc}[33m"   blue="${esc}[34m";   purple="${esc}[35m"
cyan="${esc}[36m";    white="${esc}[37m"

blackb="${esc}[40m";   redb="${esc}[41m";    greenb="${esc}[42m"
yellowb="${esc}[43m"   blueb="${esc}[44m";   purpleb="${esc}[45m"
cyanb="${esc}[46m";    whiteb="${esc}[47m"

boldon="${esc}[1m";    boldoff="${esc}[22m"
italicson="${esc}[3m"; italicsoff="${esc}[23m"
ulon="${esc}[4m";      uloff="${esc}[24m"
invon="${esc}[7m";     invoff="${esc}[27m"

reset="${esc}[0m"

CURRENT_STR=""

function cecho () {
  message=${1:-$default_msg}    # Defaults to default message.
  color=${2:-$black}            # Defaults to black, if not specified.
  newline=${3:-yes}             # Newline?

  CURRENT_STR="$color$message$reset"
  if [ "$newline" == "yes" ]; then
    echo -e $CURRENT_STR
    CURRENT_STR=""
  fi
}  

function aligned_msg () {
  answer=${1:-"?"}    # Defaults to default message.
  color=${2:-$green}
  printf "%-35s$color%s$reset\n" $CURRENT_STR "$answer"
  CURRENT_STR=""
}
function not_found_msg () {
  aligned_msg "not found" $red
}
function found_msg () {
  aligned_msg "found" $green
}

# Fix for OSX not finding malloc.h
if [[ `uname -s` == "Darwin" ]]; then
    export CFLAGS=-I/usr/include/malloc
fi

# if [[ !(-d "./build") ]]; then
#     mkdir ./build
# fi
NEOTOMA_DIR="`pwd`/priv/neotoma"
echo ${NEOTOMA_DIR}
neotoma_git_url="http://github.com/seancribbs/neotoma.git"
cecho "Neotoma..." $blue no
if [ -d "${NEOTOMA_DIR}" ]; then
  found_msg
else
  not_found_msg
  cecho "Building Neotoma" $green
  pushd `dirname ${NEOTOMA_DIR}`
  $GIT clone ${neotoma_git_url}
  pushd ${NEOTOMA_DIR}
  make
  popd
  popd
fi

AUTOCONF_VERSION=2.65
cecho "autoconf..." $blue no
if [ -n "$(which autoconf | grep $AUTOCONF_VERSION)" ]; then
  not_found_msg
  cecho "Downloading and building autoconf $AUTOCONF_VERSION"
  wget http://ftp.gnu.org/gnu/autoconf/autoconf-$AUTOCONF_VERSION.tar.gz
  tar xvzf autoconf-$AUTOCONF_VERSION.tar.gz
  pushd autoconf-$AUTOCONF_VERSION
  ./configure --prefix=/usr
  make
  sudo make install
  popd
  rm -rf autoconf-$AUTOCONF_VERSION.tar.gz autoconf-$AUTOCONF_VERSION
else
  found_msg
fi

# Run autoconf
cecho "Running autoconf..." $green
autoconf
cecho "Configuring... " $green
./configure
if [ "$?" != "0" ]; then
  cecho "Error configuring..." $red
fi
cecho "Making... " $green no
make
cecho "Running tests... " $green no
make test
if [ "$?" != "0" ]; then
  cecho "Error making..." $red
else
  cecho "success" $green
fi

# cleanup
rm -rf build/*.tar.gz
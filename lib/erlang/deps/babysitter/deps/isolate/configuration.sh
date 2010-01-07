#!/bin/sh

# $Id: configuration.sh 10 2009-03-10 04:47:06Z snackypants $

upper()
{
      echo "$1" | tr a-z A-Z
}


## MAIN

uname=$(uname)

if [ "FreeBSD" = $uname ]
then
      echo "
      const string LD_ELF_SO_PATH = \"/libexec/ld-elf.so.1\";
      const string MDMFS = \"/sbin/mdmfs\";
      "
elif [ "Linux" = $uname ]
then
      echo "
      const string DEV_RANDOM = \"/dev/urandom\";
      const string LD_ELF_SO_PATH = \"/lib/ld-linux.so.2\";
      "
else
      echo "
      Unfortunately, isolate has not yet been ported to $uname.
      Please email Chris Palmer <chris@isecpartners.com> to help!
      "
      exit 1
fi

has_glibc=$(echo "#include <stdio.h>" | g++ -E -dM -  | grep __GLIBC__ | wc -l)
if [ "$:{has_glibc}" != ":0" -a "Linux" = $uname ]
then
echo "/* GNU libc present */"
echo "
#define has_glibc 1

const string NSS_COMPAT=\"/lib/libnss_compat.so.2\";
const string NSS_DNS=\"/lib/libnss_dns.so.2\";
const string NSS_FILES=\"/lib/libnss_files.so.2\";
const string LIBRESOLV = \"/lib/libresolv.so.2\";
const string NSSWITCH_CONF = \"/etc/nsswitch.conf\";
"
fi

echo "
const static bool DEBUG = true;
const string DEFAULT_CONFINEMENT_ROOT = \"/var/isolation\";
string CONFINEMENT_ROOT;
const mode_t CONFINEMENT_ROOT_MODE = 040755;
const string DEFAULT_PATH = \"/bin:/usr/bin:/usr/local/bin\";
const string RESOLV_CONF = \"/etc/resolv.conf\";
const string TERMCAP = \"/usr/share/misc/termcap.db\";
const rlim_t DEFAULT_MEMORY_LIMIT = 0x2000000;
"

for p in chown cp mount rm rmdir umount xauth
do
      echo "const string $(upper $p) = \"$(which $p)\";"
done


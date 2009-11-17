#!/bin/sh -x

cd `dirname ../$0`
DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
if [ ! -f ebin/router.boot ]; then
	make boot
fi

progname=$(basename $0)
version="0.1"

print_usage() {
cat <<'EOF'
	Usage: $progname options

OPTIONS
	-m		Mnesia directory (defaults to ./db)
	-n 		Name of the erlang process (useful for multiple nodes on the same instance)
	-s, --slim	Do not run the rest server
	-p, --port	Port to run the router
	-d		Daemonize the process
	-h, --help	Show this screen
EOF
}

print_version() {
cat <<EOF
$progname $version

Copyright (C) 2009 Ari Lerner
EOF
}

# Defaults
HOSTNAME='hostname'
MNESIA_DIR='"./db"'
DAEMONIZE_ARGS=""
NAME="router@$HOSTNAME"
ROUTER_OPTS="-router"

SHORTOPTS="hm:n:dsp:"
LONGOPTS="help,version,port"

if $(getopt -T >/dev/null 2>&1) ; [ $? = 4 ] ; then # New longopts getopt.
    OPTS=$(getopt -o $SHORTOPTS --long $LONGOPTS -n "$progname" -- "$@")
else # Old classic getopt.
    # Special handling for --help and --version on old getopt.
    case $1 in --help) print_usage ; exit 0 ;; esac
    case $1 in --version) print_version ; exit 0 ;; esac
    OPTS=$(getopt $SHORTOPTS "$@")
fi

if [ $? -ne 0 ]; then
    echo "'$progname --help' for more information" 1>&2
    exit 1
fi

eval set -- "$OPTS"
while [ $# -gt 0 ]; do
   : debug: $1
   case $1 in
		--help)
			usage
			exit 0
			;;
		-n|--name)
			NAME=$2
			shift 2;;
		-s|--slim)
			ROUTER_OPTS="$ROUTER_OPTS run_beehive false"
			shift;;
		-p|--port)
			ROUTER_OPTS="$ROUTER_OPTS client_port $2"
			shift 2;;
		--)
			shift
			break;;
		*)
			echo "Uh oh!"
			print_usage; exit 0
			;;
	esac
done

echo $MNESIA_DIR

erl -pa $PWD/ebin \
    -pz $PWD/deps/*/ebin \
    -s reloader \
		-mnesia dir $MNESIA_DIR \
		-name $NAME \
		$ROUTER_OPTS \
    -boot router-0.1
#!/bin/sh

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
	-r, --rest	Run the rest server (boolean)
	-s, --seed	Pass in the seed node
	-p, --port	Port to run the router
	-t, --type	Type of node to start (default: router)
	-d		Daemonize the process
	-v		Verbose
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
HOSTNAME=`hostname`
MNESIA_DIR="./db"
DAEMONIZE_ARGS=""
NAME="router@$HOSTNAME"
ROUTER_OPTS="-router"
TYPE="router"
REST="true"
VERBOSE=false

SHORTOPTS="hm:n:dp:t:r:s:v"
LONGOPTS="help,version,port,type,rest,seed,mnesia_dir"

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
		-m|--mnesia_dir)
			MNESIA_DIR=$2
			shift 2;;
		-r|--rest)
			ROUTER_OPTS="$ROUTER_OPTS run_rest_server $2"
			shift 2;;
		-p|--port)
			ROUTER_OPTS="$ROUTER_OPTS client_port $2"
			shift 2;;
		-s|--seed)
			ROUTER_OPTS="$ROUTER_OPTS seed $2"
			shift 2;;
		-t|--type)
			TYPE=$2
			shift 2;;
		-v)
			VERBOSE=true
			shift;;
		--)
			shift
			break;;
		*)
			echo "Uh oh!"
			print_usage; exit 0
			;;
	esac
done

ROUTER_OPTS="$ROUTER_OPTS node_type $TYPE "


erl -pa $PWD/ebin \
    -pz $PWD/deps/*/ebin \
    -s reloader \
		-mnesia dir \'$MNESIA_DIR\' \
		-name $NAME \
		$ROUTER_OPTS \
    -boot router-0.1
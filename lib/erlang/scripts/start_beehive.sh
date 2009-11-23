#!/bin/sh

progdir=$(dirname $0)
progname=$(basename $0)
version="0.1"

if [ ! -f ebin/router-*.boot ]; then
	make && make boot
fi

print_usage() {
cat <<EOF
Usage: $progname options

Start beehive

OPTIONS
	-m		Mnesia directory (defaults to ./db)
	-n 		Name of the erlang process (useful for multiple nodes on the same instance)
	-r, --rest	Run the rest server (boolean)
	-s, --seed	Pass in the seed node
	-p, --port	Port to run the router
	-t, --type	Type of node to start (default: router)
	-g, --strategy	Strategy to choose a backend. (default: random)
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
HOSTNAME=`hostname -f`
MNESIA_DIR="./db"
DAEMONIZE_ARGS=""
ROUTER_OPTS="-router"
TYPE="router"
REST="true"
VERBOSE=false
STRATEGY="random"

SHORTOPTS="hm:n:dp:t:g:r:s:v"
LONGOPTS="help,version,port,type,strategy,rest,seed,mnesia_dir,daemonize"

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
			ROUTER_OPTS="$ROUTER_OPTS seed '$2'"
			shift 2;;
		-t|--type)
			TYPE=$2
			shift 2;;
    -g|--strategy)
			ROUTER_OPTS="$ROUTER_OPTS backend_strategy '$2'"
      shift 2;;
		-d|--daemonize)
			DAEMONIZE_ARGS="-detached -heart"
			shift;;
		-v)
			VERBOSE=true
			shift;;
		--)
			shift
			break;;
		*)
			print_usage; exit 0
			;;
	esac
done

if [ -z $NAME ]; then
	NAME="$TYPE@$HOSTNAME"
fi

if [ $TYPE != 'router' ]; then
	ROUTER_OPTS="$ROUTER_OPTS run_rest_server false"
fi

ROUTER_OPTS="$ROUTER_OPTS node_type $TYPE "

if $VERBOSE; then
cat <<EOF
	Running with:
		Mnesia dir: \'$MNESIA_DIR\'
		Name: 		\'$NAME\'
		Router opts:	$ROUTER_OPTS
EOF
fi

erl -pa $PWD/ebin \
    -pz $PWD/deps/*/ebin \
    -s reloader \
		-mnesia dir \'$MNESIA_DIR\' \
		-name $NAME \
		$ROUTER_OPTS \
		$DAEMONIZE_ARGS \
    -boot router-0.1

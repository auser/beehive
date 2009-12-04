#!/bin/sh

progdir=$(dirname $0)
progname=$(basename $0)
grep=$(which grep)
version="0.1"

if [ ! -f ebin/beehive-*.boot ]; then
	make && make boot
fi

print_usage() {
cat <<EOF
Usage: $progname options

Start beehive

OPTIONS
	-m			Mnesia directory (defaults to ./db)
	-n 			Name of the erlang process (useful for multiple nodes on the same instance)
	-r, --rest		Run the rest server (boolean)
	-a, --additional_path	Additional paths for the beehive runtime
	-c, --callback_module	Module name of the callback module
	-q, --bee_picker	Name of the method that contains the bee chooser
	-s, --seed		Pass in the seed node
	-l, --log_path		Path of the logs
	-p, --port		Port to run the router
	-i, --initial_bees 	Initial bees to start the bee_srv
	-t, --type		Type of node to start (default: router)
	-g, --strategy		Strategy to choose a bee. (default: random)
	-z, --repos_path		Git repos path
	-d			Daemonize the process
	-v			Verbose
	-h, --help		Show this screen
	
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

BEEHIVE_OPTS="-beehive"
ROUTER_OPTS="-router"
NODE_OPTS="-node"
STORAGE_OPTS="-storage"

TYPE="router"
REST="true"
VERBOSE=false
STRATEGY="random"
PATHS="-pa $PWD/ebin -pa $PWD/include"
ERL_OPTS="-s reloader +Bc +K true -smp enable"

SHORTOPTS="hm:n:dp:t:g:r:s:vi:a:c:q:l:z:"
LONGOPTS="help,version,port,type,strategy,rest,seed,mnesia_dir,daemonize,initial_bees,additional_path,callback_module,bee_picker,log_path,repos_path"

if $(getopt -T >/dev/null 2>&1) ; [ $? = 4 ] ; then # New longopts getopt.
	OPTS=$(getopt -o "$SHORTOPTS" --longoptions "$LONGOPTS" -n "$progname" -- "$@")
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
   case "$1" in
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
		-l|--log_path)
			BEEHIVE_OPTS="$BEEHIVE_OPTS log_path '$2'"
			shift 2;;
		-r|--rest)
			ROUTER_OPTS="$BEEHIVE_OPTS run_rest_server $2"
			shift 2;;
		-p|--port)
			ROUTER_OPTS="$ROUTER_OPTS client_port $2"
			shift 2;;
		-s|--seed)
			if [ $(echo $2 | $grep '@') ]; then
				SEED=$2
			else
				SEED="$2@$HOSTNAME"
			fi
			BEEHIVE_OPTS="$BEEHIVE_OPTS seed $SEED"
			shift 2;;
		-q|--bee_picker)
			ROUTER_OPTS="$ROUTER_OPTS bee_picker '$2'"
			shift 2;;
		-a|--additional_path)
			PATHS="$PATHS -pa $2"
			shift 2;;
		-c|--callback_module)
			ROUTER_OPTS="$BEEHIVE_OPTS user_defined_event_handler $2"
			shift 2;;
		-i|--initial_bees)
			ROUTER_OPTS="$ROUTER_OPTS bees '$2'"
			shift 2;;
		-t|--type)
			TYPE=$2
			shift 2;;
    -g|--strategy)
			ROUTER_OPTS="$ROUTER_OPTS bee_strategy '$2'"
      shift 2;;
		-d|--daemonize)
			DAEMONIZE_ARGS="-detached -heart"
			ERL_OPTS=""
			shift;;
		-z|--repos_path)
			STORAGE_OPTS="$STORAGE_OPTS git_repos_path '$2'"
			shift 2;;
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

# Sanity checks
if [ -z $NAME ]; then
	NAME="$TYPE@$HOSTNAME"
fi

if [ $TYPE != 'router' ]; then
	ROUTER_OPTS="$ROUTER_OPTS run_rest_server false"
fi

BEEHIVE_OPTS="$BEEHIVE_OPTS node_type $TYPE "

if [ $TYPE == 'router' ]; then
	APP_OPTS=$ROUTER_OPTS
elif [ $TYPE == 'node' ]; then
	APP_OPTS=$NODE_OPTS
elif [ $TYPE == 'storage' ]; then
	APP_OPTS=$STORAGE_OPTS
fi

if $VERBOSE; then
cat <<EOF
	Running with:
		Erlang opts: $ERL_OPTS
		Mnesia dir: '$MNESIA_DIR'
		Name: 		'$NAME'
		Beehive opts: $BEEHIVE_OPTS
		App opts: $APP_OPTS
		Paths: $PATHS
EOF
fi

erl $PATHS \
    -pz $PWD/deps/*/ebin \
    $ERL_OPTS \
		-mnesia dir \'$MNESIA_DIR\' \
		-name $NAME \
		$BEEHIVE_OPTS \
		$APP_OPTS \
		$DAEMONIZE_ARGS \
    -boot beehive-$version

/**
 * $Id: help.h 12 2010-01-05 11:19:02Z ioerror $
 */

const string HELP_MESSAGE =
"Usage: isolate [-hTvMS] [-D path] [-a bytes] [-c bytes] [-d bytes] [-f files]\n"
"               [-m bytes] [-n bytes] [-p processes] [-r bytes] [-s bytes]\n"
"               [-C path] [-i img] [-e env_string] -b [dir]\n"
"               [-t seconds] [-z bytes] program [program-arguments...]\n"
"\n OPTIONS\n"
" -h            show this message\n"
" -T            set the cookie as trusted for X11 authentication\n"
" -v            turn on verbose printing\n"
" -M            use mdmfs utility to mount an in-memory filesystem using the md(4) driver\n"
" -S            save the isolation path (do not remove it after the isolation has ended)\n"
" -D [path]     append paths to copy and load files\n"
" -a [bytes]    limit the address space to [bytes]\n"
" -c [bytes]    limit the maximum size core file the process can create to [bytes]\n"
" -d [bytes]    limit the maximum size of data memory for the process to [bytes]\n"
" -f [num]      limit the number of open files the process can have open to [num]\n"
" -m [bytes]    limit the maximum amount of memory that can be in physical memory to [bytes]\n"
" -n [bytes]    limit the maximum size in bytes of a socket buffer this process can create to [bytes]\n"
" -p [num]      limit the process to a maximum of [num] process at once\n"
" -r [bytes]    limit the maximum resident set size to [bytes]\n"
" -s [bytes]    limit the maximum size of the stack to [bytes]\n"
" -C [path]     store the confinement path to [path]\n"
" -b [path]     use this path as a root directory to copy from, rather than create a new one\n"
" -i [img]      mount the loopback [img] file to /home\n"
" -e [str]      set an environment variable using a string of the format PARAM=value\n"
" -t [seconds]  set the maximum amount of CPU time the process can consume\n"
" -z [bytes]    limit the maximum size in bytes of a file that may be created\n"
;


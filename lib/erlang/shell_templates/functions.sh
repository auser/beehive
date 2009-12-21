# Colors
red='\E[1;31m';
green='\E[1;32m';
black='\E[1;37;38m';
white='e[1;37m';
normal='tput sgr0';

function say {
  echo -n -e $black "$1"; $normal;
}
function ok {
  echo -e $green 'OK'; $normal;
}
function error {
  echo -e $red "$1"; $normal;
}
#!/usr/bin/env sh

apt-get update
apt-get -y dist-upgrade
apt-get -y install apache2-utils

echo "174.129.187.40 app.remote_beehive.com remote_beehive.com watson.remote_beehive.com test.remote_beehive.com" >> /etc/hosts

mkdir loadtest
cd loadtest
TESTS="list1 list2 list3 list4"
COOKIE="-C _session_id=12345678901234567890"
HOST="http://test.remote_beehive.com:8080/"
AB="ab -n 10000 -c 16" # 10,000 requests, concurrency 16 per process
$AB -g list1.log $COOKIE "${HOST}/1" &
$AB -g list2.log $COOKIE "${HOST}/2" &
$AB -g list3.log $COOKIE "${HOST}/3" &
$AB -g list4.log $COOKIE "${HOST}/4" &
wait
for test in $TESTS; do
    awk -F '\t' "{print \$4 \",\" \$5 \",\" \$6 \",$test\"}" < $test.log | tail -n +2 > $test
done
echo "dtime,ttime,wait,test" > bench.csv
cat $TESTS >> bench.csv
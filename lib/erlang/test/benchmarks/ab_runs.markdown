Static html
===
Static html on a single watson instance

    ab -n 10000 -c 10 http://ec2-67-202-21-173.compute-1.amazonaws.com:8080/
    Completed 1000 requests
    Completed 2000 requests
    Completed 3000 requests
    Completed 4000 requests
    Completed 5000 requests
    Completed 6000 requests
    Completed 7000 requests
    Completed 8000 requests
    Completed 9000 requests
    Completed 10000 requests
    Finished 10000 requests

    This is ApacheBench, Version 2.3 <$Revision: 655654 $>
    Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
    Licensed to The Apache Software Foundation, http://www.apache.org/

    Benchmarking ec2-67-202-21-173.compute-1.amazonaws.com (be patient)


    Server Software:        ip-10-244-51-80:8080
    Server Hostname:        ec2-67-202-21-173.compute-1.amazonaws.com
    Server Port:            8080

    Document Path:          /
    Document Length:        21 bytes

    Concurrency Level:      10
    Time taken for tests:   195.576 seconds
    Complete requests:      10000
    Failed requests:        0
    Write errors:           0
    Non-2xx responses:      10000
    Total transferred:      2520000 bytes
    HTML transferred:       210000 bytes
    Requests per second:    51.13 [#/sec] (mean)
    Time per request:       195.576 [ms] (mean)
    Time per request:       19.558 [ms] (mean, across all concurrent requests)
    Transfer rate:          12.58 [Kbytes/sec] received

    Connection Times (ms)
                  min  mean[+/-sd] median   max
    Connect:       85   92  25.6     90    1079
    Processing:    90  103  15.5     99     548
    Waiting:       68  102  12.5     99     482
    Total:        177  195  30.1    192    1202

    Percentage of the requests served within a certain time (ms)
      50%    192
      66%    196
      75%    200
      80%    202
      90%    209
      95%    215
      98%    223
      99%    231
     100%   1202 (longest request)

Static html on a single watson through the proxy (with one down instance)

    ab -n 10000 -c 10 http://watson.remote_beehive.com:8080/
    This is ApacheBench, Version 2.3 <$Revision: 655654 $>
    Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
    Licensed to The Apache Software Foundation, http://www.apache.org/

    Benchmarking watson.remote_beehive.com (be patient)
    Completed 1000 requests
    Completed 2000 requests
    Completed 3000 requests
    Completed 4000 requests
    Completed 5000 requests
    Completed 6000 requests
    Completed 7000 requests
    Completed 8000 requests
    Completed 9000 requests
    Completed 10000 requests
    Finished 10000 requests


    Server Software:        ip-10-244-51-80:8080
    Server Hostname:        watson.remote_beehive.com
    Server Port:            8080

    Document Path:          /
    Document Length:        21 bytes

    Concurrency Level:      10
    Time taken for tests:   204.391 seconds
    Complete requests:      10000
    Failed requests:        0
    Write errors:           0
    Non-2xx responses:      10000
    Total transferred:      2520000 bytes
    HTML transferred:       210000 bytes
    Requests per second:    48.93 [#/sec] (mean)
    Time per request:       204.391 [ms] (mean)
    Time per request:       20.439 [ms] (mean, across all concurrent requests)
    Transfer rate:          12.04 [Kbytes/sec] received

    Connection Times (ms)
                  min  mean[+/-sd] median   max
    Connect:       85   92   5.5     91     180
    Processing:    94  112 109.1    104    3132
    Waiting:       93  111 109.0    104    3132
    Total:        180  204 109.4    197    3228

    Percentage of the requests served within a certain time (ms)
      50%    197
      66%    201
      75%    205
      80%    208
      90%    217
      95%    227
      98%    243
      99%    263
     100%   3228 (longest request)

We can look at the shortest request on both and see an astronomically tiny difference in response time at 2 ms. The longest request, at 3228 for the proxy is far greater than the longest one for the straight watson 1202, but abstracting out both of these anomalies, the approximate average between the difference of the requests are 12 ms. 
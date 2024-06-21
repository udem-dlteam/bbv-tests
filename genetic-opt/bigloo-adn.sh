#!/bin/bash

benchmarks="micro/ack.scm micro/fib.scm micro/bague.scm micro/fibfp.scm micro/nqueens.scm micro/primes.scm micro/tak.scm"
#benchmarks="micro/ack.scm micro/fib.scm"
ADN=$@
stat="`dirname $0`/bigloo-adn-stat.sh"
jobs=

res=0

if [ "$jobs" = "1" ]; then
  for p in $benchmarks; do
    >&2 echo -n "$p "
    tmp=`$stat $p $adn`
    >&2 echo "$tmp"
    res=`echo "$res + $tmp" | bc -l`
  done
else
  if [ "$jobs " = " " ]; then
    nbprocs=`cat /proc/cpuinfo | grep processor | wc -l`
    jobs=`echo "$nbprocs/2" | bc`
  fi

  export ADN
  scores=`parallel -j $jobs $stat ::: $benchmarks`

  while IFS= read -r score; do
    res=`echo "$res + $score" | bc`
  done <<< "$scores"
fi

echo $res

# genetic-opt/genetic-opt.sh -l 12 genetic-opt/bigloo-adn.sh 2> /tmp/LOG.genetic

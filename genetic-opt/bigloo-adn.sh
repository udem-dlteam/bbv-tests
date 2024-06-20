#!/bin/bash

benchmarks="micro/ack.scm micro/fib.scm micro/bague.scm micro/fibfp.scm micro/nqueens.scm micro/primes.scm micro/tak.scm"
adn=$@

base() {
  bench=$1
  cmd=$2
  cd bigloo
   
  TMPDIR=`mktemp -d`
  TMPFILE=`mktemp -p $TMPDIR`

  cmd="bigloo -srfi arithmeticG -w -unsafe -saw -O3 bbv.bgl ../tests/paper/$bench -copt -DSAW_BBV_STATS=1 -o $TMPFILE.out"
  #>&2 echo $cmd
  
  sh -c "$cmd" || exit 1
  result=`$TMPFILE.out 2>&1 | tail +2 | bigloo -eval "(let ((l (port->sexp-list (current-input-port)))) (print (apply + (filter-map (lambda (x) (if (memq (car x) '(ifne add/ov sub/o mul/ov)) (cadr x))) l))) (exit 0))"`

  rm -rf $TMPDIR
  cd ..
  echo "$result"
}
  
bbv() {
  bench=$1
  cd bigloo
   
  TMPDIR=`mktemp -d`
  TMPFILE=`mktemp -p $TMPDIR`

  cmd="BIGLOOBBVSTRATEGY=\"adn\" BIGLOOBBVADN=\"$adn\" BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/$bench -copt -DSAW_BBV_STATS=1 -o $TMPFILE.out"
  #>&2 echo $cmd
  
  sh -c "$cmd" || exit 1
  result=`$TMPFILE.out 2>&1 | tail +2 | bigloo -eval "(let ((l (port->sexp-list (current-input-port)))) (print (apply + (filter-map (lambda (x) (if (memq (car x) '(ifne add/ov sub/o mul/ov)) (cadr x))) l))) (exit 0))"`

  rm -rf $TMPDIR
  cd ..
  echo $result
}

res=0
for p in $benchmarks; do
  >&2 echo -n "$p "
  baseCount=$(base $p)
  bbvCount=$(bbv $p)
  tmp=`echo "$bbvCount/$baseCount" | bc -l`
  >&2 echo "$tmp"
  res=`echo "$res + $tmp" | bc -l`
done

ires=`echo "$res*1000" | bc -l | sed s'/[.][0-9]*//'`
echo $ires

#parallel run ::: $benchmarks

# genetic-opt/genetic-opt.sh -l 8 genetic-opt/bigloo-adn.sh

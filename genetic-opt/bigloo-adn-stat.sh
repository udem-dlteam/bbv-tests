#!/bin/sh

benchmark=$1
shift
adn=$@

bglfiles="bigloo/bbv.bgl bigloo/bbv.sch bigloo/bbv.scm bigloo/bbv_saw.h"

if [ "$adn " = " " ]; then
  adn=$ADN
fi

if [ "$adn " = " " ]; then
  echo "*** ERROR:bigloo-adn-stat.sh -- no adn provided"
  exit 1
fi

install() {
  TMPDIR=$1
  
  for p in $bglfiles tests/paper/$bench; do
    cp $p $TMPDIR
  done
}

base() {
  bench=$1
  benchfile=`basename $bench`
   
  TMPDIR=`mktemp -d`

  install $TMPDIR
  
  cmd="(cd $TMPDIR; bigloo -srfi arithmeticG -w -saw bbv.bgl $benchfile -copt -DSAW_BBV_STATS=1 -o $benchfile.out)"
  >&2 echo $cmd
  
  sh -c "$cmd" || exit 1
  result=`$TMPDIR/$benchfile.out 2>&1 | tail +2 | bigloo -eval "(let ((l (port->sexp-list (current-input-port)))) (print (apply + (filter-map (lambda (x) (if (memq (car x) '(ifne add/ov sub/o mul/ov)) (cadr x))) l))) (exit 0))"`

  rm -rf $TMPDIR
  echo "$result"
}
  
bbv() {
  bench=$1
  benchfile=`basename $bench`
   
  TMPDIR=`mktemp -d`

  install $TMPDIR
  
  cmd="(cd $TMPDIR; BIGLOOBBVSTRATEGY=\"adn\" BIGLOOBBVADN=\"$adn\" BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl $benchfile -copt -DSAW_BBV_STATS=1 -o $benchfile.out)"
  >&2 echo $cmd
  
  sh -c "$cmd" || exit 1
  result=`$TMPDIR/$benchfile.out 2>&1 | tail +2 | bigloo -eval "(let ((l (port->sexp-list (current-input-port)))) (print (apply + (filter-map (lambda (x) (if (memq (car x) '(ifne add/ov sub/o mul/ov)) (cadr x))) l))) (exit 0))"`

  rm -rf $TMPDIR
  echo $result
}

baseCount=$(base $benchmark)
bbvCount=$(bbv $benchmark)

tmp=`echo "$bbvCount/$baseCount" | bc -l`
echo "$tmp*1000" | bc -l | sed s'/[.][0-9]*//'

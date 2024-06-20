#!/bin/sh

# This script receives the genome in the parameters and must
# produce a single integer as output. For example:
#
# $ ./evaluate.sh 1 2 3 4 5 6 7 8
# -646

echo $@ | gsi -e "(let loop ((i (* 1000 (random-real)))) (if (> i 0) (loop (- i 1))))(println (apply (lambda (a b c d e f g h) (- (+ (square a) (square b) (square c) (square d)) (square (+ e f g h)))) (read-all)))"

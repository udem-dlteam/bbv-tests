#!/bin/sh

# This script receives the genome in the parameters and must
# produce a single integer as output. For example:
#
# $ ./evaluate.sh 1 2 3 4 5 6 7 8
# -646

echo $@ | gsi -e '(println (apply (lambda (a b c d e f g h) (- (+ (square a) (square b) (square c) (square d)) (square (+ e f g h)))) (read-all)))'

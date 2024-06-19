#!/bin/sh

GENOME_LENGTH=8
GENOME_RANGE_LO=-100
GENOME_RANGE_HI=+100
GENOME_MUTATION_LO=-10
GENOME_MUTATION_HI=+10

POPULATION_SIZE=10
POPULATION_SURVIVORS=3

NB_GENERATIONS=200

evaluate_genome()
{
  genome="$3"

  # compute "genome -> evaluation"
  # for testing: compute some random function on the genes
  export genome
  evaluation=`gsi -e "(println (apply (lambda (a b c d e f g h) (- (+ (square a) (square b) (square c) (square d)) (square (+ e f g h)))) (with-input-from-string (getenv \"genome\") read-all)))"`

  : $(($1 = evaluation))
}

rand()
{
  : $((_seed = _SEED$2))
  : $((_hi = 2147483647 & (_seed / 2787)))
  : $((_lo = 2147483647 & (_seed % 2787)))
  : $((_new_seed = 2147483647 & ((2147483647 & (_lo * 2813)) - (2147483647 & (_hi * 2699)))))
  : $(($1 = _SEED$2 = _new_seed))
}

_SEED0=1085320881

_i=$((RANDOM / 100))
while [ $_i -gt 0 ] ; do
  rand _r 0
  : $((_i -= 1))
done

cycle_length()
{
  start=$1
  _SEED0=$start
  n=0
  while : ; do
    rand _r 0
    if [ $_r = $start ] ; then
      break
    fi
    : $((n += 1))
    if [ $((n % 1000)) = 0 ] ; then
      echo $n $_r
    fi
  done
  echo $start length=$n
}

random_in_range_inclusive()
{
  : $((_n = $4 - $3 + 1))
  rand _r $2
  : $(($1 = (_r % _n) + $3))
}

random_genome()
{
  genome=
  : $((_i = 0))
  while [ $_i -lt $GENOME_LENGTH ] ; do
    random_in_range_inclusive _gene $1 $GENOME_RANGE_LO $GENOME_RANGE_HI
    genome="$genome $_gene"
    : $((_i += 1))
  done
}

random_population()
{
  : $((_j = 0))
  while [ $_j -lt $POPULATION_SIZE ] ; do
    random_genome $1
    rand _r $1
    eval "_GENOME$_j=\"$genome\""
    evaluate_genome _s $1 "$genome"
    : $((_SCORE$_j = _s))
    : $((_j += 1))
  done
}

sort_population()
{
  : $((_i = 0))
  while [ $_i -lt $POPULATION_SIZE ] ; do
    : $((_min_pos = _i))
    : $((_j = _i+1))
    while [ $_j -lt $POPULATION_SIZE ] ; do
      if [ $((_SCORE$_j)) -lt $((_SCORE$_min_pos)) ] ; then
        _min_pos=$_j
      fi
      : $((_j += 1))
    done
    : $((_tmp_score = _SCORE$_i))
    eval "_tmp_genome=\$_GENOME$_i"
    : $((_SCORE$_i = _SCORE$_min_pos))
    eval "_GENOME$_i=\$_GENOME$_min_pos"
    : $((_SCORE$_min_pos = _tmp_score))
    eval "_GENOME$_min_pos=\"$_tmp_genome\""
    : $((_i += 1))
  done
}

cross_genomes()
{
  : $((_k = 0))
  for _gene in $2 ; do
    : $((_t$_k = _gene))
    : $((_k += 1))
  done
  : $((_k = 0))
  for _gene in $3 ; do
    rand _r $1
    if [ $((_r % 1000)) -ge 500 ] ; then
      : $((_t$_k = _gene))
    fi
    rand _r $1
    if [ $((_r % 1000)) -lt 100 ] ; then
      random_in_range_inclusive _mut $1 $GENOME_MUTATION_LO $GENOME_MUTATION_HI
      : $((_val = _t$_k + _mut))
      if [ $_val -lt $GENOME_RANGE_LO ] ; then
        _val=$GENOME_RANGE_LO
      elif [ $_val -gt $GENOME_RANGE_HI ] ; then
        _val=$GENOME_RANGE_HI
      fi
      : $((_t$_k = _val))
    fi
    : $((_k += 1))
  done
  genome=
  : $((_k = 0))
  while [ $_k -lt $GENOME_LENGTH ] ; do
    genome="$genome $((_t$_k))"
    : $((_k += 1))
  done
}

breed_population()
{
  : $((_i = POPULATION_SURVIVORS))
  while [ $_i -lt $POPULATION_SIZE ] ; do
    rand _r $1
    : $((_x = _r % POPULATION_SURVIVORS))
    rand _r $1
    : $((_y = (_x + 1 + _r % (POPULATION_SURVIVORS-1)) % POPULATION_SURVIVORS))
    eval "cross_genomes $1 \"\$_GENOME$_x\" \"\$_GENOME$_y\""
    eval "_GENOME$_i=\"$genome\""
    evaluate_genome _s $1 "$genome"
    : $((_SCORE$_i = _s))
    : $((_i += 1))
  done
}

show_population()
{
  echo ============================================== generation: $generation
  : $((_j = 0))
  while [ $_j -lt $POPULATION_SIZE ] ; do
    eval "echo $((_SCORE$_j)) : \$_GENOME$_j"
    : $((_j += 1))
  done
}

random_population 0
sort_population
show_population

generation=0
while [ $generation -lt $NB_GENERATIONS ] ; do
  breed_population 0
  sort_population
  show_population
  : $((generation += 1))
done

#!/bin/sh

# This script performs a search for a genome (a set of integer parameters)
# that minimizes some evaluation function computed by a shell script.
#
# Sample use:
#
# $ ./genetic-opt.sh -l 8 -g 100 ./evaluate.sh
# ...

usage()
{
  echo "Usage: $0 [options] <evaluation_script>"
  echo "Options:"
  echo "  -l <genome_length>        Select genome length (default=1)"
  echo "  -lo <gene_lo_value>       Select gene lo value (default=-100)"
  echo "  -hi <gene_hi_value>       Select gene hi value (default=+100)"
  echo "  -mlo <gene_mut_lo_value>  Select gene mutation lo value (default=-10)"
  echo "  -mhi <gene_mut_hi_value>  Select gene mutation hi value (default=+10)"
  echo "  -mut <mutation_percent>   Select mutation percentage (default=10)"
  echo "  -pop <population_size>    Select population size (default=10)"
  echo "  -s <nb_of_survivors>      Select number of survivors (default=3)"
  echo "  -g <nb_of_generations>    Select number of generations (default=100)"
  echo "  -p                        Evaluate in parallel (default is sequential)"
  echo "  -h                        Help"
}

# Default parameters
GENOME_LENGTH=1
GENOME_RANGE_LO=-100
GENOME_RANGE_HI=+100
GENOME_MUTATION_LO=-10
GENOME_MUTATION_HI=+10

POPULATION_SIZE=10
POPULATION_SURVIVORS=3

NB_GENERATIONS=100

MUTATION_PERCENTAGE=20

PARALLEL=no

EVALUATION_SCRIPT=

# Parse parameters
while [ $# -ge 1 ] ; do
  case $1 in
    -l|--l)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -l <genome_length>"
        usage
        exit 1
      fi
      GENOME_LENGTH=$2
      shift
      ;;
    -lo|--lo)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -lo <gene_lo_value>"
        usage
        exit 1
      fi
      GENOME_LO_VALUE=$2
      shift
      ;;
    -hi|--hi)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -hi <gene_hi_value>"
        usage
        exit 1
      fi
      GENOME_HI_VALUE=$2
      shift
      ;;
    -mlo|--mlo)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -mlo <gene_mut_lo_value>"
        usage
        exit 1
      fi
      GENOME_MUTATION_LO_VALUE=$2
      shift
      ;;
    -mhi|--mhi)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -mhi <gene_mut_hi_value>"
        usage
        exit 1
      fi
      GENOME_MUTATION_HI_VALUE=$2
      shift
      ;;
    -mut|--mut)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -mut <mutation_percentage>"
        usage
        exit 1
      fi
      MUTATION_PERCENTAGE=$2
      shift
      ;;
    -pop|--pop)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -pop <population_size>"
        usage
        exit 1
      fi
      POPULATION_SIZE=$2
      shift
      ;;
    -s|--s)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -s <nb_of_survivors>"
        usage
        exit 1
      fi
      POPULATION_SURVIVORS=$2
      shift
      ;;
    -g|--g)
      if [ $# -lt 2 ] ; then
        echo "*** option needs an integer parameter: -g <nb_of_generations>"
        usage
        exit 1
      fi
      NB_GENERATIONS=$2
      shift
      ;;
    -p|--p)
      PARALLEL=yes
      ;;
    -h|--h)
      usage
      exit 0
      ;;
    -*)
      usage
      exit 1
      ;;
    *)
      if [ "$EVALUATION_SCRIPT" != "" ] ; then
        echo "*** please specify a single <evaluation_script>"
        usage
        exit 1
      fi
      EVALUATION_SCRIPT=$1
      ;;
  esac
  shift
done

if [ "$EVALUATION_SCRIPT" = "" ] ; then
  echo "*** please specify an <evaluation_script>"
  usage
  exit 1
fi

evaluate_genome()
{
  genome="$3"

  # compute "genome -> evaluation"
  evaluation=`$EVALUATION_SCRIPT $genome`

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
    : $((_SCORE$_j = 999999999))
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
    if [ $((_r % 1000)) -lt $((10*MUTATION_PERCENTAGE)) ] ; then
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
    : $((_SCORE$_i = 999999999))
    : $((_i += 1))
  done
}

evaluate_population()
{
  genomes=
  : $((_i = $2))
  while [ $_i -lt $POPULATION_SIZE ] ; do
    eval "genomes=\"\$genomes \\\"\$_GENOME$_i\\\"\""
    : $((_SCORE$_i = _s))
    : $((_i += 1))
  done
  eval "evaluate_population_aux $genomes"
  : $((_i = $2))
  for _s in `cat genetic-opt.evaluations` ; do
    : $((_SCORE$_i = _s))
    : $((_i += 1))
  done
  if [ $_i != $POPULATION_SIZE ] ; then
    echo "*** inconsistent number of evaluations"
    exit 1
  fi
}

evaluate_population_aux()
{
  if [ $PARALLEL = "yes" ] ; then
    parallel -k "$EVALUATION_SCRIPT {}" ::: "$@" > genetic-opt.evaluations
  else
    rm -f genetic-opt.evaluations
    for genome in "$@" ; do
      eval "$EVALUATION_SCRIPT $genome" >> genetic-opt.evaluations
    done
  fi
}

show_population()
{
  echo ============================================== generation: $generation
  : $((_j = 0))
  while [ $_j -lt $POPULATION_SIZE ] ; do
    eval "echo score=$((_SCORE$_j)) : \$_GENOME$_j"
    : $((_j += 1))
  done
}

random_population 0
evaluate_population 0 0
sort_population

generation=0
while [ $generation -lt $NB_GENERATIONS ] ; do
  show_population
  breed_population 0
  evaluate_population 0 $POPULATION_SURVIVORS
  sort_population
  : $((generation += 1))
done

show_population

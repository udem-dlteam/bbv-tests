# bbv-tests
Collection of programs to test BBV

## To compile with Bigloo

  ./compile --bigloo tests/recursive/fib.scm
  
To compile with another Bigloo version:

  BIGLOODIR=$pdir ./compile --bigloo tests/recursive/fib.scm  
  
To compile with statistics

  BIGLOOOPT="-copt -DSAW_BBV_STATS=1" ./compile --bigloo tests/recursive/fib.scm  
  BIGLOODIR=$pdir BIGLOOOPT="-copt -DSAW_BBV_STATS=1" ./compile --bigloo tests/recursive/fib.scm  

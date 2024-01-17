# bbv-tests

Collection of programs to test BBV

## To compile with Bigloo

  ../compile -v -S bigloo -V 1 -f ../tests/recursive/fib.scm

### To install Bigloo

```
mkdir -p bigloo/download
mkdir -p bigloo/local
wget http://www-sop.inria.fr/indes/fp/Bigloo/download/bigloo-unstable.tar.gz -O bigloo/download/bigloo-unstable.tar.gz
(ROOT=$PWD; cd bigloo/download; tar xvfz bigloo-unstable.tar.gz; cd bigloo-unstable; ./configure --prefix=$ROOT/bigloo/local && make && make install)
```

On M2 macOS, it might be needed to disable the unistring support with:

(cd bigloo-unstable; ./configure --prefix=$ROOT/bigloo/local --disable-unistring && make && make install)


### To compile with another Bigloo version:

  ../compile -v -D $pdir -S bigloo -V 4 -f ../tests/recursive/fib.scm
  
### To compile with statistics

  BIGLOOOPT="-copt -DSAW_BBV_STATS=1" ./compile --bigloo tests/recursive/fib.scm
  BIGLOOOPT="-copt -DSAW_BBV_STATS=1" ./compile -D $pdir --bigloo tests/paper/macro/boyer.scm

This generates an output such as:

```
***primitive-call-counter
(ifne 44657499)
(eq 16748604)
(lefx 0)
(gtfx 3)
(ltfx 0)
(addfx 5580144)
(subfx 11164376)
(mulfx 0)
(div 0)
(add/ov 0)
(sub/ov 0)
(mul/ov 0)
```

### To control the number of versions per basic block:

  export BIGLOOBBVVERSIONLIMIT=num 
  
Example:
  BIGLOOBBVVERSIONLIMIT=4 BIGLOODIR=$pdir BIGLOOOPT="-copt -DSAW_BBV_STATS=1" ./compile --bigloo tests/recursive/fib.scm

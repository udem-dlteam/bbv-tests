# bbv-tests

Collection of programs to test BBV

## To compile

use the `compile` script:

```
Usage: ./compile [options]
Options:
  -f <filename>     File to compile
  -S <system>       Specify the system ('gambit' or 'bigloo')
  -D <dir>          Specify compiler directory
  -V <limit>        Set the version limit
  -O <level>        Set optimization level (1, 2, 3)
  -U                Use unsafe arithmetic
  -P                Count primitive usage
  -G                Generate CFG
  -W                View CFG in pdf viewer
  -I                Use igsc (gambit only)
  -c                Clean up tests
  -h                Display this help and exit
  -v                Execute this script in verbose mode
```

#### Note on compilation

- At the moment, these is a single optimization level, both `-O2` and `-O3` will fallback to `-O1`.

- Bigloo and gambit behave differently with `-P`. Gambit will use its GVM interpreter and iwll output primitive count. In the case of Bigloo, the resulting executable must be ran to get primitive count.

- If Bigloo is installed from the instructions below, it is not required to provide it with a `-D` parameter.


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

  ./compile -D $pdir --bigloo tests/recursive/fib.scm
  
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

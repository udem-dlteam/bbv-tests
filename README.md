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

```
./compile -S bigloo -D $pdir -V 4 -f tests/recursive/fib.scm
```

### To compile with statistics

```
./compile -S bigloo -D $pdir -V 4 -f tests/recursive/fib.scm -P
```

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

### To compile manually

```
cd bigloo
BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 /home/serrano/prgm/project/bigloo/bigloo/bin/bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/macro/maze.scm 
```

To add dynamic type check count, add the following compilation option

```
-copt -DSAW_BBV_STATS=1
```

To sum all the counter:

```
./a.out |& tail +2 | bigloo -eval "(begin (print (apply + (map cadr (port->sexp-list (current-input-port))))) (exit 0))" 
```

For instance:

```
BIGLOOBBVSTRATEGY="adn" BIGLOOBBVADN="1 0.5 1.2 3.4 5.6 6.7" BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 /home/serrano/prgm/project/bigloo/bigloo/bin/bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/macro/maze.scm -copt -DSAW_BBV_STATS=1
./a.out |& tail +2 | bigloo -eval "(begin (print (apply + (map cadr (port->sexp-list (current-input-port))))) (exit 0))" 
```

To generate a json dump for one particular function:

```
cd bigloo
BIGLOOBBVDUMPJSON=true BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 /home/serrano/prgm/project/bigloo/bigloo/bin/bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/macro/maze.scm  -fsaw-bbv-fun pick-entrances
```

To generate a json dump and a profile the generate code:

```
cd bigloo
BIGLOOSAWPROFILE=true BIGLOOBBVDUMPJSON=true BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 /home/serrano/prgm/project/bigloo/bigloo/bin/bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/macro/maze.scm  -fsaw-bbv-fun pick-entrances
./a.out > prof.json
```

Logs are easier to understand if basic blocks are not cleanup after the BBV optimization. 
So, it might help to compile with:

```
cd bigloo
BIGLOOBBVCLEANUP=false BIGLOOSAWPROFILE=true BIGLOOBBVDUMPJSON=true BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 /home/serrano/prgm/project/bigloo/bigloo/bin/bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/macro/maze.scm  -fsaw-bbv-fun pick-entrances
./a.out > prof.json
```

To combine add profiling information to the json dump:

```
./addusages.mjs bbv-pick-entrances.bbv.json prof.json | js-beautify > bbv-pick-entrances.prof.json
```

To generate a dot graph file

```
cd bigloo
BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 /home/serrano/prgm/project/bigloo/bigloo/bin/bigloo -srfi arithmeticG -w -unsafe -saw -O3 -fsaw-bbv bbv.bgl ../tests/paper/macro/maze.scm  -fsaw-bbv-fun pick-entrances -t2
```

and finally to produce a PDF file

```
cd bigloo
bglcfg 'bbv-pick-entrances.bbv.cfg' > 'bbv-pick-entrances.bbv.dot' && dot 'bbv-pick-entrances.bbv.dot' -Tpdf > 'bbv-pick-entrances.bbv.pdf'
```

### To execute manually compiled benchmark

Call the executable with command line arguments as scheme keywords:

```
/path/to/executable/benchmark.exe [arguments]
```

Example:

```
/path/to/executable/benchmark.exe repeat: 2 n: 39.0
```

This will execute the call `(run n: 39.0)` within the benchmark and repeat it twice.

For node:

```
node maze.js '{"repeat": 2, "n": 50000}'
```

### Execute benchmarks with Chez Scheme

```
cat ./chez/bbv.scm ./tests/paper/micro/fib.scm ./chez/main.scm > fib.bundle.scm
chez --quiet --optimize-level 2 --script fib.bundle.scm
```

### Test a benchmark

```
cd benchmarks

python -m venv venv               # Only required once
. ./venv/bin/activate             # Only required once
pip install -r requirements.txt   # Only required once

./test_benchmark [options]
```

To see `./test_benchmark` options us `./test_benchmark -h`:

```
Usage: ./test_benchmark.sh [options]
Options:
  -f <filename>    File to compile
  -S <system>      Specify the system ('gambit', 'bigloo', 'chez', 'racket', 'node')
  -D <dir>         Specify compiler directory
  -V <limits>...   Set the version limits (default: 0 1 5 10)
  -O <level>       Set optimization level (0 or 1)
  -U               Use unsafe arithmetic
  -h               Display this help and exit
  -v               Execute this script in verbose mode
```

The ouput looks like this:

```
- almabench (bigloo, V=0)
  Compilation         : OK
  Counting Primitives : OK
  Execution           : OK
  Computing Size      : OK
  Internal Error      : OK
```

Where each line indicate whether the benchmark script was able to compile, count primitive, execute the benchmark without error and compute the size of the execuatble respectively. The last line, Internal Error, indicate whether an unexpected error happened during the test.

## Visualization tools

The `visual-sbbv` folder contains a webapp to visualize the result of SBBV, open `visual-sbbv/index.html` in the browser (I tested Firefox only).


It reads a JSON of the form:

```json
{
    "compiler": "gambit",
    "specializedCFG":[
        {
            "id": 436,
            "origin": 10,
            "bbs": "exec-bench",
            "source": "(cdr (command-line))",
            "usage": 0,
            "context": "[#ret|rt . . .] r1=#",
            "predecessors": [435],
            "successors": [435],
            "references": [123],
            "ret": [456],
            "jumps":[{
              "bbs":"exec-bench",
              "id":118,
              "count":1
            }]
            "details":"#436 fs=4   <- #435   [#ret|rt #123 . .] r1=#\n  jump fs=4 #<primitive ##dead-end> r0=#435 nargs=0 [#ret|rt . . .] r1=#"
        }
    ],
    "history": [
        {
            "event": "create",
            "bbs": "exec-bench",
            "origin": 4,
            "id": 123,
            "context": "[#ret|rt fx . .] r1=#"
        },
        {
            "event": "merge",
            "bbs": "exec-bench",
            "origin": 4,
            "merged": [123, 234],
            "id": 235,
            "context": "[#ret|rt . . .] r1=#"
        },
        {
            "event": "request",
            "bbs": "exec-bench",
            "origin": 4,
            "id": 123,
        },
        {
            "event": "replace",
            "bbs": "exec-bench",
            "origin": 4,
            "from": 123,
            "id": 235,
        }
        {
            "event": "unreachable",
            "bbs": "exec-bench",
            "origin": 4,
            "id": 124
        },
        {
            "event": "reachable",
            "bbs": "exec-bench",
            "origin": 4,
            "id": 236
        }
    ]
}
```

The `"compiler"` field should be `"gambit"` or `"bigloo"` and the `"specializedCFG"` should be a list of specialized basic block.

The required keys for a specialized basic block are:

- `id`: label of the bb;
- `origin`: label of the unspecialized bb which this bb is a version of;
- `bbs`: the procedure name or identifier of the bbs;
- `source`: source code corresponding to the bb;
- `usage`: how many time the bb was entered at execution;
- `context`: string representation of the type context when entering the bb;
- `predecessors`: a list of `id` of blocks which can jump to this bb;
- `successors`: a list of `id` to which this block can jump to;
- `references`: (optional) a list of `id` to which this block has references (for instance a label being moved to a register);
- `ret`: (optional) if this block does a call with a return address, this list contains the return address;
- `jumps`: (optional) a list of jumps from this block at execution. A jump is an object with an `id`, a `bbs` and a `count`. This allows tracing hot paths at execution;
- `details`: a string that must contain some representation of the code executed by this bb, for instance Gambit outputs GVM code. It can contain some more information. The tool will display the `details` as is in a `<code>` element.

To visualize the merge history, the json must contain a `"history"` field which is a list of events. Events must be in chronological order in the list. Each event has these common fields:

- `event`: a string (`"create"`, `"merge"`, `"request"`, `"replace"`, `"reachable"` or `"unreachable"`);
- `bbs`: the procedure name or identifier of the bbs;
- `origin`: label of the unspecialized bb on which the event takes place;

Only the `"create"` and `"merge"` events are required. Other events are there to allow a finer-grained description of the algorithm.

Here is the meaning of each event:

 - `"create"`: a context was requested and a new specialized block was created for that exact context;
 - `"merge"`: merge of some versions to another version;
 - `"request"`: a specific context was requested, but a specialized block already existed for it (either nothing happened of the block was unreachable and made reachable anew);
 - `"replace"`: some specialized block was replaced by an replacement because its context has previsouly been merged to that replacement. This event allows a fine-grained description of `"merge"` and `"request"` events where a block was created but the resulting block was immediately replaced by the result of a previous merge;
 - `"unreachable"` and `"reachable"`: a specialized block was made unreachable or reachable due to another manipulation (for instance a merge can make blocks unreachable).

The `"replace"` event can be added to give a fine-grained description of merges and requests. For instance, if some context is requested and had an existing block with id `123`, but that block was previsouly merged to `456`. Then a coarse-grained event chain would be:

```json
{
    "event": "request",
    "bbs": "foo",
    "origin": 1,
    "id": 456  # Result of the request
}
```

But a fine-grained history would be:

```json
{
    "event": "request",
    "bbs": "foo",
    "origin": 1,
    "id": 123   # Block of the initially requested context
},
{
    "event": "replace",
    "bbs": "foo",
    "origin": 1,
    "from": 123,
    "id": 456   # Replacement of 123 by 456
}
```

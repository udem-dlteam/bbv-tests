set -euo pipefail

# find script directory
SCRIPT_DIR="$0"
if [ "${SCRIPT_DIR#/}" = "$SCRIPT_DIR" ]; then
  SCRIPT_DIR="$(pwd)/$SCRIPT_DIR"
fi
SCRIPT_DIR="${SCRIPT_DIR%/*}"
SCRIPT_DIR="${SCRIPT_DIR%/.}"

# Default parameters
INPUT_FILE=$SCRIPT_DIR/../tests/paper/macro/almabench.scm   # file to compile
OPTIM="-O"                                                  # optimisations
SAFE_ARITHMETIC=1                                           # safe/unsafe arithmetic
COMPILER_DIR=                                               # compiler directory
SYSTEM=bigloo                                               # system (gambit or bigloo)
SCRIPT_VERBOSITY=

collecting_vl=0
# Initialize an empty array for version limits
VERSION_LIMITS=(0 1 5 10)

# Process options
while getopts ":f:S:D:V:O:UPGWIEchv" opt; do
  case $opt in
    f)
      INPUT_FILE=$(readlink -f "$OPTARG")
      ;;
    S)
      case $OPTARG in
        'gambit'|'bigloo'|'chez'|'racket'|'node')
          SYSTEM=$OPTARG
          ;;
        *)
          echo "Unknown system: $OPTARG" >&2
          exit 1
          ;;
      esac
      ;;
    D)
      COMPILER_DIR=$OPTARG
      ;;
    V)
      # Mark that we are collecting version limits
      VERSION_LIMITS=()
      collecting_vl=1
      shift $((OPTIND-2)) # Shift arguments to start collecting version limits
      while [ $# -gt 0 ]; do
        if [[ $1 == -* ]]; then
          break # Stop collecting if we encounter another option
        fi
        # Validate each argument before adding to VERSION_LIMITS
        if [[ $1 =~ ^-?[0-9]+$ ]]; then
          VERSION_LIMITS+=("$1")
        else
          echo "Version limit must be an integer or 'custom', not '$1'"
          exit 1
        fi
        shift
      done
      collecting_vl=0
      OPTIND=1 # Reset OPTIND to process the rest of the arguments correctly
      ;;
    O)
      case $OPTARG in
        0)
          OPTIM=""
          ;;
        1|2|3)
          OPTIM="-O"
          ;;
        *)
          echo "Invalid optimization level: -O$OPTARG" >&2
          exit 1
          ;;
      esac
      ;;
    U)
      SAFE_ARITHMETIC=0
      ;;
    v)
      set -x
      SCRIPT_VERBOSITY="-v"
      ;;
    h)
      echo "Usage: $0 [options]"
      echo "Options:"
      echo "  -f <filename>    File to compile"
      echo "  -S <system>      Specify the system ('gambit', 'bigloo', 'chez', 'racket', 'node')"
      echo "  -D <dir>         Specify compiler directory"
      echo "  -V <limits>...   Set the version limits (integers or 'custom')"
      echo "  -O <level>       Set optimization level (0 or 1)"
      echo "  -U               Use unsafe arithmetic"
      echo "  -h               Display this help and exit"
      echo "  -v               Execute this script in verbose mode"
      exit 0
      ;;
    \?)
      exit 1
      ;;
  esac
done

. ./venv/bin/activate

python ./benchmark.py $SCRIPT_VERBOSITY test ${INPUT_FILE} ${OPTIM} --${SYSTEM} ${COMPILER_DIR} -l "${VERSION_LIMITS[@]}"

deactiavte

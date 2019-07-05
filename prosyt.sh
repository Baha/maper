#!/bin/bash

# Usage example:
# ./full_tester.sh tests/biggest_bug.erl prop_biggest

GEN_CFG="gen_cfg.pl"
DEFAULT_NUM_TESTS=100
verbose=false
test=true
prefix="cg"
gto=false

SPATH="$(dirname $(readlink -f $0))"

trap "clean_up" SIGINT

function clean_up()
{
  rm -f $GEN_CFG
  rm -f timings.txt
  rm -f pbgen_data.txt
}

function print_help()
{
  local bold=`tput bold`
  local normal=`tput sgr0`
  local underline=`tput smul`
  local nounderline=`tput rmul`

  printf "\nUsage: ${bold}prosyt${normal} [OPTION]... SOURCE PROPERTY      \n\n"

  printf "Check whether or not the SOURCE outputs satisfy PROPERTY.        \n\n"

  printf " ${bold}--tests${normal} N                                         \n"
  printf "       specifies the number of tests to be run.                    \n"
  printf "       Default: 100                                                \n"
  printf "\n"

  printf " ${bold}--min-size${normal} M                                      \n"
  printf "       specifies the minimum size of terms generated by typeof     \n"
  printf "       Default: 0                                                  \n"
  printf "\n"

  printf " ${bold}--max-size${normal} M                                      \n"
  printf "       specifies the maximum size of terms generated by typeof     \n"
  printf "       Default: 42                                                 \n"
  printf "\n"

  printf " ${bold}--inf${normal} I                                           \n"
  printf "       specifies the minimum value for the random generator range  \n"
  printf "       Default: -1000                                              \n"
  printf "\n"

  printf " ${bold}--sup${normal} S                                           \n"
  printf "       specifies the maximum value for the random generator range  \n"
  printf "       Default: 1000                                               \n"
  printf "\n"

  printf " ${bold}--force-spec${normal}                                      \n"
  printf "       adds some constraints on the output expressions of functions\n"
  printf "       as specified by their contracts                             \n"
  printf "\n"

  printf " ${bold}--symbolic_generation-and-filter${normal}                  \n"
  printf "       executes the test cases generation process that runs the    \n"
  printf "       (symbolic) type generator first, followed by the filter     \n"
  printf "\n"

  printf " ${bold}--generation-timeout${normal} T                            \n"
  printf "       sets a timeout T in seconds for the generation process      \n"
  printf "\n"

  printf " ${bold}--skeleton-instances${normal} I                            \n"
  printf "       sets the maximum number of test cases generated from        \n"
  printf "       each symbolic answer of the filter expression               \n"
  printf "\n"

  printf " ${bold}--verbose${normal}                                         \n"
  printf "       prints statistics about timings                             \n"
  printf "       - 'erl2clp': time required for translating from Erlang to CLP\n"
  printf "       - 'tests generation': time required for generating the test cases\n"
  printf "       - 'testing': time required for running the tests             \n"
  printf "\n"

  printf " ${bold}-h${normal}, ${bold}--help${normal}                        \n"
  printf "       display this help and exit                                  \n"
  printf "\n"
}

# ------------------------------------------------------------------------------
# parse parameters
# ':'  (colon character) tells that the option has a required argument.
# '::' (two consequent colon character) tells that the option has an optional argument.
# ------------------------------------------------------------------------------
ARGS=$(getopt -o h -a \
     --long "max-size:,min-size:,tests:,inf:,sup:,force-spec,generation-only,\
             symbolic_generation-and-filter,generation-timeout:,\
             skeleton-instances:,help,verbose" -n "$0" -- "$@");

if [ $? -ne 0 ]; then
  printf "Try \`%s --help' for more information.\n" $0
  exit 1
fi

eval set -- "$ARGS";

while true; do
  case $1 in
  --tests)
    if [ $2 -gt 0 ]; then
      DEFAULT_NUM_TESTS=$2
      shift 2
    else
      echo "tests must be greater than 0"
      exit 1
    fi
  ;;
  --max-size)
    echo ":- set_config(max_size($2))." >> "$GEN_CFG"
    shift 2
  ;;
  --min-size)
    echo ":- set_config(min_size($2))." >> "$GEN_CFG"
    shift 2
  ;;
  --inf)
    if [[ $2 =~ ^[+-]?[0-9]+ ]]; then
      echo ":- set_config(int_inf($2))." >> "$GEN_CFG"
      shift 2
    else
      echo "inf must be a number"
      exit 1
    fi
  ;;
  --sup)
    if [[ $2 =~ ^[+-]?[0-9]+ ]]; then
      echo ":- set_config(int_sup($2))." >> "$GEN_CFG"
      shift 2
    else
      echo "sup must be a number"
      exit 1
    fi
  ;;
  --force-spec)
    echo ":- eval:assert(eval_option(use_spec))." >> "$GEN_CFG"
    shift
  ;;

  --generation-only)
    test=false
    shift
  ;;

  --symbolic_generation-and-filter)
    prefix="gc"
    shift
  ;;

  --generation-timeout)
    gto=true
    to=$2
    shift 2
  ;;

  --skeleton-instances)
    echo ":- set_config(skeleton_instances($2))." >> "$GEN_CFG"
    shift 2
  ;;

  --verbose)
    verbose=true
    shift
  ;;
  -h | --help)
    print_help
    shift
    exit 0
  ;;
  --)
    shift
    break
  ;;

  esac
done

if [ $# -ne 2 ]; then
  echo "Please, select a file and property."
  exit 1
fi

if [[ ! -f $1 ]]; then
  echo $1 "does not exist."
  clean_up
  exit 1
fi

/usr/bin/time -f "%U" -o timings.txt ./scripts/erl2pl.sh $1

if [ -e $GEN_CFG ]; then
  cat $GEN_CFG >> ${1%%.erl}.pl
  rm $GEN_CFG
fi

if $gto; then
  ./scripts/cmdrunner "./scripts/pbgen.sh ${1%%.erl} ${prefix}_$2 $DEFAULT_NUM_TESTS > pbgen_data.txt" $to
  if [[ $? == 143 ]]; then
    echo $to >> timings.txt
  fi
else
  ./scripts/pbgen.sh ${1%%.erl} "${prefix}_$2" $DEFAULT_NUM_TESTS > pbgen_data.txt
  if [[ $? == 42 ]]; then
    echo $2 "does not exist."
    clean_up
    exit 1
  fi
fi

if $test; then
  printf "\nTests Results: "
  cat pbgen_data.txt | /usr/bin/time -f "%U" -a -o timings.txt ./scripts/erl_tester.sh ${1%%.erl} $2
fi

if $verbose; then
  printf "\nTimings (s)\n"
  printf "%-16s | %s\n" "erl2clp" $(sed -n '1p' timings.txt)
  printf "%-16s | %s (test cases generated: %s out of %s)\n" "tests generation"\
  $(sed -n '2p' timings.txt) $(wc -l < pbgen_data.txt) $DEFAULT_NUM_TESTS
  if $test; then
    printf "%-16s | %s\n" "testing" $(sed -n '3p' timings.txt)
  fi
  rm timings.txt
fi

BASENAME=${1##*/}
rm -f ${BASENAME%%.erl}.beam

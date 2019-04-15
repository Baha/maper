#!/bin/bash

# Usage example:
# ./full_tester.sh tests/biggest_bug.erl prop_biggest

GEN_CFG="gen_cfg.pl"
DEFAULT_NUM_TESTS=10

function print_help()
{
  echo "Not yet implemented!" # TODO
}

# ------------------------------------------------------------------------------
# parse parameters
# ':'  (colon character) tells that the option has a required argument.
# '::' (two consequent colon character) tells that the option has an optional argument.
# ------------------------------------------------------------------------------
ARGS=$(getopt -o h -a \
     --long "max-size:,min-size:,tests:,range-exp:,force-spec,help"\
     -n "$0" -- "$@");

if [ $? -ne 0 ]; then
  printf "Try \`%s --help' for more information.\n" $0
  exit 1
fi

eval set -- "$ARGS";

while true; do
  case $1 in
  --max-size)
    echo ":- set_config(max_size($2))." >> "$GEN_CFG"
    shift 2
  ;;
  --min-size)
    echo ":- set_config(start_size($2))." >> "$GEN_CFG"
    shift 2
  ;;
  --tests)
    if [ $2 -gt 0 ]; then
      DEFAULT_NUM_TESTS=$2
      shift 2
    else
      echo "tests must be greater than 0"
      exit 1
    fi
  ;;
  --range-exp)
    if [ $2 -gt 0 ]; then
      echo ":- set_config(int_exp($2))." >> "$GEN_CFG"
      shift 2
    else
      echo "the exponent must be greater than 0"
      exit 1
    fi
  ;;
  --force-spec)
    echo ":- eval:assert(eval_option(use_spec))." >> "$GEN_CFG"
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

if [ $# -ne 2 ]
then
  echo "Please, select a file and property."
  exit 1
fi

./erl2pl.sh $1

if [ -e $GEN_CFG ]; then
  cat $GEN_CFG >> ${1%%.erl}.pl
  rm $GEN_CFG
fi

# ./pbgen.sh ${1%%.erl} "gen_"$2 $DEFAULT_NUM_TESTS | ./erl_tester.sh ${1%%.erl} $2
./pbgen.sh ${1%%.erl} "gen_"$2 $DEFAULT_NUM_TESTS > pbgen_data.txt
cat pbgen_data.txt | ./erl_tester.sh ${1%%.erl} $2

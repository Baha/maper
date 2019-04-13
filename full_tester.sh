#!/bin/bash

# Usage example:
# ./full_tester.sh tests/biggest_bug.erl prop_biggest

PROP_CFG="prop_types_cfg.pl"
DEFAULT_NUM_TESTS=10

# ------------------------------------------------------------------------------
# parse parameters
# ':'  (colon character) tells that the option has a required argument.
# '::' (two consequent colon character) tells that the option has an optional argument.
# ------------------------------------------------------------------------------
ARGS=$(getopt -o h -a \
     --long "max-size:,min-size:,tests:"\
     -n "$0" -- "$@");

if [ $? -ne 0 ]; then
  printf "Try \`%s --help' for more information.\n" $0
  exit 1
fi

eval set -- "$ARGS";

while true; do
  case $1 in
  --max-size)
    echo ":- set_config(max_size($2))." >> "$PROP_CFG"
    shift 2
  ;;
  --min-size)
    echo ":- set_config(start_size($2))." >> "$PROP_CFG"
    shift 2
  ;;
  --tests)
    if [ $2 -gt 0 ]; then
      DEFAULT_NUM_TESTS=$2
    else
      echo "tests must be greater than 0"
      exit 1
    fi
    shift 2
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

if [ -e $PROP_CFG ]; then
  cat $PROP_CFG >> ${1%%.erl}.pl
  rm $PROP_CFG
fi

# ./pbgen.sh ${1%%.erl} "gen_"$2 $DEFAULT_NUM_TESTS | ./erl_tester.sh ${1%%.erl} $2
./pbgen.sh ${1%%.erl} "gen_"$2 $DEFAULT_NUM_TESTS > pbgen_data.txt
cat pbgen_data.txt | ./erl_tester.sh ${1%%.erl} $2

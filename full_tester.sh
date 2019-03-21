#!/bin/bash

# Usage example:
# ./full_tester.sh tests/biggest_bug.erl prop_biggest

if [ $# -ne 2 ]
then
  echo "Please, select a file and property."
  exit 1
fi

DEFAULT_NUM_TESTS=10

./erl2pl.sh $1
# ./pbgen.sh ${1%%.erl} "gen_"$2 $DEFAULT_NUM_TESTS | ./erl_tester.sh ${1%%.erl} $2
./pbgen.sh ${1%%.erl} "gen_"$2 $DEFAULT_NUM_TESTS > pbgen_data.txt
cat pbgen_data.txt | ./erl_tester.sh ${1%%.erl} $2

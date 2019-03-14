#!/bin/bash

if [ $# -neq 2 ] 
then
  echo "Please, select a file and property."
  exit 1
fi

DEFAULT_NUM_TESTS=10

./erl2pl.sh $1".erl"
./pbgen.sh $1.pl $2 $DEFAULT_NUM_TESTS
./erl_tester.sh $1 $2

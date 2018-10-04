#!/bin/bash

if [ -z $1 ] 
then
  echo "Please, choose a file"
  exit 1
fi

erl -noshell -pa ebin -run erl2fact main $1 -s init stop > ${1%%.erl}.pl


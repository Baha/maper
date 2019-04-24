#!/bin/bash

if [ -z $1 ]
then
  echo "Please, choose a file"
  exit 1
fi

SPATH="$(dirname $(readlink -f $0))"
EBIN="${SPATH%/scripts}/ebin"
DEST="${1%%.erl}.pl"

echo ":- style_check(-singleton)." > $DEST

erl -noshell -pa $EBIN -run erl2fact main $1 -s init stop >> $DEST
erl -noshell -pa $EBIN -run symgen main $1 -s init stop >> $DEST

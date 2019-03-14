#!/bin/bash
# USAGE example ./pbgen.sh tests/delete_bug gen_list_and_elem1 10


if [ -f "$1".pl ]; then
  file="$1"
else
  echo "Please, choose a file or folder"
  exit 1
fi

# possibly strip .pl suffix


numoftests=$3
#if ! [[ $numoftests =~ '^[0-9]+$' ]] ; then
#   numoftests=1
#fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
# now=$(date "+%Y-%m-%d %H:%M:%S")

swipl -l "./src/rnd_gen.pl" --quiet -t "run" "$DIR/$file" $2 $numoftests   # >>  $logfile

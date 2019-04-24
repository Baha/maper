#!/bin/bash
# USAGE example ./pbgen.sh ../tests/delete_bug gen_list_and_elem1 10

SPATH="$(dirname $(readlink -f $0))"

if [ -f "$1".pl ]; then
  file="${SPATH%/scripts}/$1"
else
  echo "Please, choose a file or folder"
  exit 1
fi
prop=$2
ntests=$3

swipl -l "${SPATH%/scripts}/src/randgen.pl" --quiet -t "run" "$file" $prop $ntests

#!/bin/bash


export SUBS_CONFIG=$(pwd)/default.ini

export PATH="$(pwd)/test:$PATH"

IFS='
'
for i in $(ls ./test/t*.sh)
do
  $i
done

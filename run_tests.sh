#!/bin/bash



export PATH="$(pwd)/test:$PATH"

IFS='
'
for i in $(ls ./test/t*.sh)
do
  $i || exit 1
done

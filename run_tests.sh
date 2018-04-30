#!/bin/bash


export SUBS_CONFIG=$(pwd)/default.ini

export PATH="$(pwd)/test:$PATH"

./test/basic.sh

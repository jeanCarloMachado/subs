#!/bin/bash


export SUBS_CONFIG=$(pwd)/default.ini

export PATH="$PATH:$(pwd)/test"


./test/basic.sh

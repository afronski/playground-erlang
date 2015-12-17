#!/bin/sh

ROOT=`pwd`
DIR=./erts-6.1/bin
sed s:%FINAL_ROOTDIR%:$ROOT: $DIR/erl.src > $DIR/erl

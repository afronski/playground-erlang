#!/bin/sh

./erts-6.1/bin/erl \
    -sname cache \
    -boot ./releases/0.1.0/start \
    -config ./releases/0.1.0/sys \
    -detached

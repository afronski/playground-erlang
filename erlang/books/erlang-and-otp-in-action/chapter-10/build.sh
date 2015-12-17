#!/bin/bash

erlc -o ./simple_cache/ebin ./simple_cache/src/*.erl
erlc -o ./resource_discovery/ebin ./resource_discovery/src/*.erl

erl -noshell -pa ./simple_cache/ebin -pa ./resource_discovery/ebin -eval 'systools:make_script("simple_cache", [local])' -s init stop

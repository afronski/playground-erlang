#!/bin/bash

erlc -o ./tcp_interface/ebin ./tcp_interface/src/*.erl
erlc -o ./gen_web_server/ebin ./gen_web_server/src/*.erl
erlc -pa ./gen_web_server/ebin -o ./http_interface/ebin ./http_interface/src/*.erl
erlc -o ./simple_cache/ebin ./simple_cache/src/*.erl
erlc -o ./resource_discovery/ebin ./resource_discovery/src/*.erl

erl -noshell -pa ./simple_cache/ebin -pa ./resource_discovery/ebin -eval 'systools:make_script("simple_cache", [local])' -s init stop

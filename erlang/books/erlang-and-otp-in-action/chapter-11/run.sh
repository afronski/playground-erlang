#!/bin/bash

erl -sname contact1 -detached
erl -sname mynode -pa ./tcp_interface/ebin -pa ./gen_web_server/ebin -pa ./http_interface/ebin -pa ./simple_cache/ebin -pa ./resource_discovery/ebin/ -boot ./simple_cache -config sys

#!/bin/sh -e
MAIN=sample_server
erlc -I include -pa ebin -o ebin examples/$MAIN.erl
erl -pa ebin -noshell -sasl errlog_type error -run $MAIN start $@

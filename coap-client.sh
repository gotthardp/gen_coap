#!/bin/sh -e
MAIN=sample_client
erlc -I include -pa ebin -o ebin examples/$MAIN.erl
erl -pa ebin -noshell -sasl errlog_type error -run $MAIN start $@

#!/bin/bash
while True; do
    rebar3 eunit && rebar3 ct
    if [ "$?" -ne "0" ]; then
        exit 1
    fi
done

#!/bin/bash

rm -rf ./*.beam

erlc bench.erl
erl -sname "benchmark@localhost" -noinput -eval "bench:start()"
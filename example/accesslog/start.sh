#!/bin/bash

rm -rf ./*.beam

erlc accesslog.erl
erl -sname localhost -noinput -eval "accesslog:start('localhost')"
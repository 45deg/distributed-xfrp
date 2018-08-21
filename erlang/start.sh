#!/bin/bash

rm -rf ./*.beam

erlc $1.erl
erl -sname localhost -noinput -eval "$1:start('localhost')"
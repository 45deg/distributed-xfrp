#!/bin/bash

rm -rf ./*.beam

erlc watchdog.erl
erl -sname localhost -noinput -eval "watchdog:start('localhost')"
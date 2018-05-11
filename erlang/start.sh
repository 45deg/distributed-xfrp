#!/bin/bash

rm -rf ./*.beam

for OPT in "$@"
do
  case $OPT in
  '-nodebug')
    NODEBUG=1
    ;;
  *)
    erlc $1.erl
    if [ "$NODEBUG" ]; then
      echo " c($1). $1:main()." | erl
    else
      echo " c($1). dbg:tracer(). dbg:tpl($1, '_', []). dbg:p(new, m). $1:main()." | erl
    fi
    ;;
  esac
  shift
done

#!/bin/bash

program="../main.native"

for fname in *.xfrp; do
  fbname=${fname%.*}
  tmpl=$fbname.tmpl
  erl=../erlang/${fbname#test/}.erl
  $program $fname -o ${erl/test_/} -t $tmpl $@
done
#!/bin/bash

rm -rf cecho
git clone https://github.com/45deg/cecho.git
cd cecho
git checkout 45deg-patch
./rebar3 clean
./rebar3 compile
cd ..

../../main.native board.xfrp -t board_io.erl -o board.erl

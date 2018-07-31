git clone https://github.com/mazenharake/cecho
cd checho
./rebar3 clean
./rebar3 compile
cd ..

../main.native board.xfrp -t board_io.erl -o board.erl

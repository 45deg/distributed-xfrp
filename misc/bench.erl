-module(bench).

-export([start/0]).
-export([benchmark/1]).

benchmark({Actor, Message}) -> 
  io:format("~p,~p,~p~n", [erlang:system_time(),Actor, Message]),
  receive 
    message -> benchmark({Actor, Message+1});
    add_actor -> benchmark({Actor+1, Message});
    del_actor -> benchmark({Actor-1, Message})
  end.

start() -> 
	register(benchmark, spawn(?MODULE, benchmark, [{0,0}])).
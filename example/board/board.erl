-module(board).

-export([start/1]).
-export([out/2,out_node/2,in/1,wait/1]).
-export([fun_assoc_update/3,fun_movebykey/2,fun_update_buffer/4]).
-export([key1/1,key2/1]).
-export([unified@0/2]).
-export([pos1/4,pos2/4,buffer/4,board/4]).

-define(SORTBuffer, fun ({{K1, V1}, _}, {{K2, V2}, _}) -> if
	V1 == V2 -> K1 < K2;
	true -> V1 < V2
end end).
buffer_update(Current, Rest, {Id, RValue, {RVId, RVersion}}, Buffer) ->
	H1 = case lists:member(Id, Current) of
		true  -> maps:update_with({RVId, RVersion}, fun(M) -> M#{ Id => RValue } end, #{ Id => RValue }, Buffer);
		false -> Buffer
	end,
	case lists:member(Id, Rest) of
		true  -> maps:update_with({RVId, RVersion + 1}, fun(M) -> M#{ {last, Id} => RValue } end, #{ {last, Id} => RValue }, H1);
		false -> H1
	end.
buffer_update_l(Current, Rest, {Id, RValue, {RVId, RVersion}}, Buffer, Latest) ->
	Buf = buffer_update(Current, Rest, {Id, RValue, {RVId, RVersion}}, Buffer),
	maps:filter(fun({I,Version},_) -> Version >= element(1,maps:get(I,Latest,{0,0})) end, Buf).
wait(Hosts) -> 
	case lists:all(fun (N) -> net_kernel:connect_node(N) end, Hosts) of
		true -> ok;
		false -> timer:sleep(1000), wait(Hosts)
	end.
resender(T, Target, Msg) ->
	receive ack -> ok
	after T -> Target ! Msg, resender(T, Target, Msg) end.
update_lvpair(Version, ESet) ->
	case sets:is_element(Version + 1, ESet) of
		true  -> update_lvpair(Version + 1, sets:del_element(Version + 1, ESet));
		false -> {Version, ESet}
	end.
update_lv({Id, Version}, M) ->
	{Num, ESet} = maps:get(Id, M, {0, sets:new()}),
	maps:put(Id, case Version - 1 of
		Num -> update_lvpair(Version, ESet);
		_ -> {Num, sets:add_element(Version, ESet)}
	end, M).
is_new_lv({Id, Version}, M) -> maps:is_key(Id, M) andalso element(1,maps:get(Id, M)) < Version.

fun_assoc_update(Vkey,Vvalue,Vlist) -> (case Vlist of [] -> [{Vkey,Vvalue}]; [{Vk,Vv}|Vls] -> (case (Vk == Vkey) of true -> [{Vkey,Vvalue}|Vls]; false -> [{Vk,Vv}|fun_assoc_update(Vkey,Vvalue,Vls)] end) end).

fun_movebykey(Vpos,Vkey) -> (case Vpos of {Vx,Vy} -> (case Vkey of 258 -> {(Vx + 1),Vy}; 259 -> {(case (Vx == 0) of true -> Vx; false -> (Vx - 1) end),Vy}; 260 -> {Vx,(case (Vy == 0) of true -> Vy; false -> (Vy - 1) end)}; 261 -> {Vx,(Vy + 1)}; _ -> {Vx,Vy} end) end).

fun_update_buffer(Vplayer,Vpos,Vkey,Vbuffer) -> (case ((32 =< Vkey) andalso (Vkey =< 126)) of true -> fun_assoc_update(Vpos,{Vplayer,Vkey},Vbuffer); false -> Vbuffer end).

start('client1@localhost') -> 
	register(unified@0, spawn(?MODULE, unified@0, [0, #{}])),
	register(pos1, spawn(?MODULE, pos1, [#{{unified@0, 0} => #{{last, pos1} => {0,0}}}, #{}, [], #{}])),
	register(key1, spawn(?MODULE, key1, [0])),
	wait(['client2@localhost','server@localhost']),
	in('client1@localhost');
start('client2@localhost') -> 
	register(pos2, spawn(?MODULE, pos2, [#{{unified@0, 0} => #{{last, pos2} => {0,0}}}, #{}, [], #{}])),
	register(key2, spawn(?MODULE, key2, [0])),
	wait(['client1@localhost','server@localhost']),
	in('client2@localhost');
start('server@localhost') -> 
	register(board, spawn(?MODULE, board, [#{{unified@0, 0} => #{}}, #{}, [], #{}])),
	register(buffer, spawn(?MODULE, buffer, [#{{unified@0, 0} => #{{last, buffer} => []}}, #{}, [], #{}])),
	register(out_node,spawn(?MODULE, out_node, ['server@localhost', #{}])),
	wait(['client1@localhost','client2@localhost']),
	in('server@localhost');
start(_) -> erlang:error(badarg).

-include_lib("cecho/include/cecho.hrl").

init_cecho() ->
  application:start(cecho),
  cecho:start_color(),
  cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
  cecho:init_pair(2, ?ceCOLOR_WHITE, ?ceCOLOR_RED),
  cecho:init_pair(3, ?ceCOLOR_BLUE, ?ceCOLOR_BLACK),
  cecho:init_pair(4, ?ceCOLOR_WHITE, ?ceCOLOR_BLUE),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE),
  cecho:keypad(?ceSTDSCR, true),
  cecho:move(0,0),
  cecho:refresh().

in(Host) -> % init
  init_cecho(),
  in(Host, main).

in('client1@localhost', main) ->
  key1 ! 0,
  cecho:mvaddstr(0,0,"CLIENT 1"),
	cecho:refresh(),
	key1 ! cecho:getch(),
  in('client1@localhost', main);
in('client2@localhost', main) ->
  key2 ! 0,
  cecho:mvaddstr(0,0,"CLIENT 2"),
	cecho:refresh(),
 	key2 ! cecho:getch(),
	in('client2@localhost', main);
in('server@localhost', main) ->
  receive _ -> ok end,
	in('server@localhost', main);
in(_, _) -> erlang:error(badarg).

putch(P, C, X, Y) ->
  cecho:attron(?ceCOLOR_PAIR(P)),
  cecho:mvaddch(X,Y,C),
  cecho:attroff(?ceCOLOR_PAIR(P)).

out(board, {[{X1,Y1},{X2,Y2}],Board}) -> 
  cecho:erase(),
  putch(2, $\s, X1, Y1), % cursor
  putch(4, $\s, X2, Y2), % cursor
  lists:foreach(fun ({{X, Y}, {P, C}}) -> 
    case {P,X,Y} of
      {1, X1, Y1} -> putch(2, C, X1, Y1); % cursor with char
      {2, X2, Y2} -> putch(4, C, X2, Y2); % cursor with char
      _ -> putch(2*P-1, C, X, Y)
    end
  end, Board),
	cecho:refresh();
out(_, _) -> erlang:error(badarg).

key1(Version) ->
	receive
	Value ->
	R1 = spawn(fun () -> resender(100, {unified@0,'client1@localhost'}, {{key1, [{buffer,'server@localhost'},{pos1,'client1@localhost'}], Value}, self()}) end), (case (rand:uniform() > 0.1) of true -> {unified@0,'client1@localhost'} ! {{key1, [{buffer,'server@localhost'},{pos1,'client1@localhost'}], Value}, R1}; false -> io:format(standard_error, "Dropped ~p~n", [{{key1, [{buffer,'server@localhost'},{pos1,'client1@localhost'}], Value}, R1}]) end)
	end,
	key1(Version + 1).

key2(Version) ->
	receive
	Value ->
	R1 = spawn(fun () -> resender(100, {unified@0,'client1@localhost'}, {{key2, [{buffer,'server@localhost'},{pos2,'client2@localhost'}], Value}, self()}) end), (case (rand:uniform() > 0.1) of true -> {unified@0,'client1@localhost'} ! {{key2, [{buffer,'server@localhost'},{pos2,'client2@localhost'}], Value}, R1}; false -> io:format(standard_error, "Dropped ~p~n", [{{key2, [{buffer,'server@localhost'},{pos2,'client2@localhost'}], Value}, R1}]) end)
	end,
	key2(Version + 1).

unified@0(Version, Last) ->
	Elements = receive
		{{Source, Targets, Value}, R0} -> (case (rand:uniform() > 0.1) of true -> R0 ! ack; false -> io:format(standard_error, "Dropped ~p~n", [ack]) end), Last#{Source => {Targets, Value}}
	end,
	case maps:size(Elements) of
		2 -> 
			maps:map(fun (Source, {Targets, Value}) -> 
				lists:foreach(fun (Target) -> R = spawn(fun () -> resender(100, Target, {{Source, Value, {unified@0, Version}}, self()}) end), (case (rand:uniform() > 0.1) of true -> Target ! {{Source, Value, {unified@0, Version}}, R}; false -> io:format(standard_error, "Dropped ~p~n", [{{Source, Value, {unified@0, Version}}, R}]) end) end, Targets)
			end, Elements),
			unified@0(Version + 1, Elements);
		_ -> unified@0(Version, Elements)
	end.

out_node('server@localhost', Latest) ->
	NLatest = receive
		{{board, Value, Version}, R} -> (case (rand:uniform() > 0.1) of true -> R ! ack; false -> io:format(standard_error, "Dropped ~p~n", [ack]) end), is_new_lv(Version, Latest) andalso out(board, Value), update_lv(Version, Latest)
	end,
	out_node('server@localhost', NLatest);
out_node(_, Latest) -> erlang:error(badarg).

pos1(Buffer0, Rest0, Deferred0, Latest0) ->
	io:format(standard_error, "pos1(~p,~p,~p)~n", [{Buffer0, Latest0}, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred, NLatest} = lists:foldl(fun (E, {Buffer, Rest, Deferred, Latest}) -> 
		case E of
			{ {unified@0, _} = Version, #{key1 := Skey1, {last, pos1} := LSpos1} = Map} ->
				Curr = fun_movebykey(LSpos1,Skey1),
				lists:foreach(fun (V) -> 
					R1 = spawn(fun () -> resender(100, {board,'server@localhost'}, {{pos1, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {board,'server@localhost'} ! {{pos1, Curr, V}, R1}; false -> io:format(standard_error, "Dropped ~p~n", [{{pos1, Curr, V}, R1}]) end),
					R2 = spawn(fun () -> resender(100, {buffer,'server@localhost'}, {{pos1, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {buffer,'server@localhost'} ! {{pos1, Curr, V}, R2}; false -> io:format(standard_error, "Dropped ~p~n", [{{pos1, Curr, V}, R2}]) end),
					R3 = spawn(fun () -> resender(100, {pos1,'client1@localhost'}, {{pos1, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {pos1,'client1@localhost'} ! {{pos1, Curr, V}, R3}; false -> io:format(standard_error, "Dropped ~p~n", [{{pos1, Curr, V}, R3}]) end)
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), [], update_lv(Version, Latest)};
			_ -> {Buffer, Rest, Deferred, Latest}
		end
	end, {Buffer0, Rest0, Deferred0, Latest0}, HL),
	Received = receive {{_,_,{_, _}} = M, R} -> (case (rand:uniform() > 0.1) of true -> R ! ack; false -> io:format(standard_error, "Dropped ~p~n", [ack]) end), M end,
	io:format(standard_error, "pos1 receives (~p)~n", [Received]),
	pos1(buffer_update_l([key1], [pos1], Received, NBuffer, NLatest), NRest, NDeferred, NLatest).


pos2(Buffer0, Rest0, Deferred0, Latest0) ->
	io:format(standard_error, "pos2(~p,~p,~p)~n", [{Buffer0, Latest0}, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred, NLatest} = lists:foldl(fun (E, {Buffer, Rest, Deferred, Latest}) -> 
		case E of
			{ {unified@0, _} = Version, #{key2 := Skey2, {last, pos2} := LSpos2} = Map} ->
				Curr = fun_movebykey(LSpos2,Skey2),
				lists:foreach(fun (V) -> 
					R1 = spawn(fun () -> resender(100, {board,'server@localhost'}, {{pos2, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {board,'server@localhost'} ! {{pos2, Curr, V}, R1}; false -> io:format(standard_error, "Dropped ~p~n", [{{pos2, Curr, V}, R1}]) end),
					R2 = spawn(fun () -> resender(100, {buffer,'server@localhost'}, {{pos2, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {buffer,'server@localhost'} ! {{pos2, Curr, V}, R2}; false -> io:format(standard_error, "Dropped ~p~n", [{{pos2, Curr, V}, R2}]) end),
					R3 = spawn(fun () -> resender(100, {pos2,'client2@localhost'}, {{pos2, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {pos2,'client2@localhost'} ! {{pos2, Curr, V}, R3}; false -> io:format(standard_error, "Dropped ~p~n", [{{pos2, Curr, V}, R3}]) end)
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), [], update_lv(Version, Latest)};
			_ -> {Buffer, Rest, Deferred, Latest}
		end
	end, {Buffer0, Rest0, Deferred0, Latest0}, HL),
	Received = receive {{_,_,{_, _}} = M, R} -> (case (rand:uniform() > 0.1) of true -> R ! ack; false -> io:format(standard_error, "Dropped ~p~n", [ack]) end), M end,
	io:format(standard_error, "pos2 receives (~p)~n", [Received]),
	pos2(buffer_update_l([key2], [pos2], Received, NBuffer, NLatest), NRest, NDeferred, NLatest).


buffer(Buffer0, Rest0, Deferred0, Latest0) ->
	io:format(standard_error, "buffer(~p,~p,~p)~n", [{Buffer0, Latest0}, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred, NLatest} = lists:foldl(fun (E, {Buffer, Rest, Deferred, Latest}) -> 
		case E of
			{ {unified@0, _} = Version, #{pos2 := Spos2, pos1 := Spos1, key2 := Skey2, key1 := Skey1, {last, buffer} := LSbuffer} = Map} ->
				Curr = (case {fun_update_buffer(1,Spos1,Skey1,LSbuffer)} of {Vb1} -> (case {fun_update_buffer(2,Spos2,Skey2,Vb1)} of {Vb2} -> Vb2 end) end),
				lists:foreach(fun (V) -> 
					R1 = spawn(fun () -> resender(100, {board,'server@localhost'}, {{buffer, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {board,'server@localhost'} ! {{buffer, Curr, V}, R1}; false -> io:format(standard_error, "Dropped ~p~n", [{{buffer, Curr, V}, R1}]) end),
					R2 = spawn(fun () -> resender(100, {buffer,'server@localhost'}, {{buffer, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> {buffer,'server@localhost'} ! {{buffer, Curr, V}, R2}; false -> io:format(standard_error, "Dropped ~p~n", [{{buffer, Curr, V}, R2}]) end)
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), [], update_lv(Version, Latest)};
			_ -> {Buffer, Rest, Deferred, Latest}
		end
	end, {Buffer0, Rest0, Deferred0, Latest0}, HL),
	Received = receive {{_,_,{_, _}} = M, R} -> (case (rand:uniform() > 0.1) of true -> R ! ack; false -> io:format(standard_error, "Dropped ~p~n", [ack]) end), M end,
	io:format(standard_error, "buffer receives (~p)~n", [Received]),
	buffer(buffer_update_l([pos2, pos1, key2, key1], [buffer], Received, NBuffer, NLatest), NRest, NDeferred, NLatest).


board(Buffer0, Rest0, Deferred0, Latest0) ->
	io:format(standard_error, "board(~p,~p,~p)~n", [{Buffer0, Latest0}, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred, NLatest} = lists:foldl(fun (E, {Buffer, Rest, Deferred, Latest}) -> 
		case E of
			{ {unified@0, _} = Version, #{pos2 := Spos2, pos1 := Spos1, buffer := Sbuffer} = Map} ->
				Curr = {[Spos1,Spos2],Sbuffer},
				lists:foreach(fun (V) -> 
					R1 = spawn(fun () -> resender(100, out_node, {{board, Curr, V}, self()}) end), (case (rand:uniform() > 0.1) of true -> out_node ! {{board, Curr, V}, R1}; false -> io:format(standard_error, "Dropped ~p~n", [{{board, Curr, V}, R1}]) end)
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), [], update_lv(Version, Latest)};
			_ -> {Buffer, Rest, Deferred, Latest}
		end
	end, {Buffer0, Rest0, Deferred0, Latest0}, HL),
	Received = receive {{_,_,{_, _}} = M, R} -> (case (rand:uniform() > 0.1) of true -> R ! ack; false -> io:format(standard_error, "Dropped ~p~n", [ack]) end), M end,
	io:format(standard_error, "board receives (~p)~n", [Received]),
	board(buffer_update_l([pos2, pos1, buffer], [], Received, NBuffer, NLatest), NRest, NDeferred, NLatest).

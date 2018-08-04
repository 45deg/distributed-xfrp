-module(board).

-export([start/1]).
-export([out/2,out_node/1,in/1,wait/1]).
-export([fun_assoc_update/3,fun_movebykey/2,fun_update_buffer/4]).
-export([key1/1,key2/1,unified@0/2]).
-export([pos1/3,pos2/3,buffer/3,board/3]).

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
wait(Hosts) -> 
	case lists:all(fun (N) -> net_kernel:connect_node(N) end, Hosts) of
		true -> ok;
		false -> timer:sleep(1000), wait(Hosts)
	end.

fun_assoc_update(Vkey,Vvalue,Vlist) -> (case Vlist of [] -> [{Vkey,Vvalue}]; [{Vk,Vv}|Vls] -> (case (Vk == Vkey) of true -> [{Vkey,Vvalue}|Vls]; false -> [{Vk,Vv}|fun_assoc_update(Vkey,Vvalue,Vls)] end) end).

fun_movebykey(Vpos,Vkey) -> (case Vpos of {Vx,Vy} -> (case Vkey of 258 -> {(Vx + 1),Vy}; 259 -> {(case (Vx == 0) of true -> Vx; false -> (Vx - 1) end),Vy}; 260 -> {Vx,(case (Vy == 0) of true -> Vy; false -> (Vy - 1) end)}; 261 -> {Vx,(Vy + 1)}; _ -> {Vx,Vy} end) end).

fun_update_buffer(Vplayer,Vpos,Vkey,Vbuffer) -> (case ((32 =< Vkey) andalso (Vkey =< 126)) of true -> fun_assoc_update(Vpos,{Vplayer,Vkey},Vbuffer); false -> Vbuffer end).

start('client1@localhost') -> 
	register(unified@0, spawn(?MODULE, unified@0, [0, #{}])),
	register(pos1, spawn(?MODULE, pos1, [#{{unified@0, 0} => #{{last, pos1} => {0,0}}}, #{}, []])),
	register(key1, spawn(?MODULE, key1, [0])),
	wait(['client2@localhost','server@localhost']),
	in('client1@localhost');
start('client2@localhost') -> 
	register(pos2, spawn(?MODULE, pos2, [#{{unified@0, 0} => #{{last, pos2} => {0,0}}}, #{}, []])),
	register(key2, spawn(?MODULE, key2, [0])),
	wait(['client1@localhost','server@localhost']),
	in('client2@localhost');
start('server@localhost') -> 
	register(board, spawn(?MODULE, board, [#{{unified@0, 0} => #{}}, #{}, []])),
	register(buffer, spawn(?MODULE, buffer, [#{{unified@0, 0} => #{{last, buffer} => []}}, #{}, []])),
	register(out_node,spawn(?MODULE, out_node, ['server@localhost'])),
	wait(['client1@localhost','client2@localhost']),
	in('server@localhost');
start(_) -> erlang:error(badarg).

-include_lib("cecho/include/cecho.hrl").

init_cecho() ->
  application:start(cecho),
  cecho:start_color(),
  cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
  cecho:init_pair(2, ?ceCOLOR_RED, ?ceCOLOR_RED),
  cecho:init_pair(3, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
  cecho:init_pair(4, ?ceCOLOR_BLUE, ?ceCOLOR_BLUE),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE),
  cecho:keypad(?ceSTDSCR, true),
  cecho:move(0,0),
  cecho:refresh().

in('client1@localhost') ->
  init_cecho(),
  key1 ! 0,
  cecho:mvaddstr(0,0,"CLIENT 1"),
	cecho:refresh(),
	key1 ! cecho:getch(),
	in('client1@localhost');
in('client2@localhost') ->
  init_cecho(),
  key2 ! 0,
  cecho:mvaddstr(0,0,"CLIENT 2"),
	cecho:refresh(),
 	key2 ! cecho:getch(),
	in('client2@localhost');
in('server@localhost') ->
  init_cecho(),
  receive _ -> ok end,
	in('server@localhost');
in(_) -> erlang:error(badarg).

out(board, {[{X1,Y1},{X2,Y2}],Board}) -> 
  cecho:erase(),
  lists:foreach(fun ({{X, Y}, {P, C}}) -> 
    cecho:mvaddch(X,Y,C)
  end, Board),
  cecho:attron(?ceCOLOR_PAIR(2)),
  cecho:mvaddch(X1,Y1,$I),
  cecho:attroff(?ceCOLOR_PAIR(2)),
  cecho:attron(?ceCOLOR_PAIR(4)),
  cecho:mvaddch(X2,Y2,$I),
  cecho:attroff(?ceCOLOR_PAIR(4)),
	cecho:refresh();
out(_, _) -> erlang:error(badarg).

key1(Version) ->
	receive
	Value ->
	{unified@0,'client1@localhost'} ! {key1, [{buffer,'server@localhost'},{pos1,'client1@localhost'}], Value}
	end,
	key1(Version + 1).

key2(Version) ->
	receive
	Value ->
	{unified@0,'client1@localhost'} ! {key2, [{buffer,'server@localhost'},{pos2,'client2@localhost'}], Value}
	end,
	key2(Version + 1).

unified@0(Version, Last) ->
	Elements = receive
		{Source, Targets, Value} -> Last#{Source => {Targets, Value}}
	end,
	case maps:size(Elements) of
		2 -> 
			maps:map(fun (Source, {Targets, Value}) -> 
				lists:foreach(fun (Target) -> Target ! {Source, Value, {unified@0, Version}} end, Targets)		
			end, Elements),
			unified@0(Version + 1, Elements);
		_ -> unified@0(Version, Elements)
	end.

out_node('server@localhost') ->
	receive
		{board, Value, _} -> out(board, Value)
	end,
	out_node('server@localhost');
out_node(_) -> erlang:error(badarg).

pos1(Buffer0, Rest0, Deferred0) ->
	io:format(standard_error, "pos1(~p,~p,~p)~n", [Buffer0, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Deferred}) -> 
		case E of
			{ {unified@0, _} = Version, #{key1 := Skey1, {last, pos1} := LSpos1} = Map} ->
				Curr = fun_movebykey(LSpos1,Skey1),
				lists:foreach(fun (V) -> 
					{board,'server@localhost'} ! {pos1, Curr, V},
					{buffer,'server@localhost'} ! {pos1, Curr, V},
					{pos1,'client1@localhost'} ! {pos1, Curr, V}
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), []};
			_ -> {Buffer, Rest, Deferred}
		end
	end, {Buffer0, Rest0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	io:format(standard_error, "pos1 receives (~p)~n", [Received]),
	pos1(buffer_update([key1], [pos1], Received, NBuffer), NRest, NDeferred).


pos2(Buffer0, Rest0, Deferred0) ->
	io:format(standard_error, "pos2(~p,~p,~p)~n", [Buffer0, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Deferred}) -> 
		case E of
			{ {unified@0, _} = Version, #{key2 := Skey2, {last, pos2} := LSpos2} = Map} ->
				Curr = fun_movebykey(LSpos2,Skey2),
				lists:foreach(fun (V) -> 
					{board,'server@localhost'} ! {pos2, Curr, V},
					{buffer,'server@localhost'} ! {pos2, Curr, V},
					{pos2,'client2@localhost'} ! {pos2, Curr, V}
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), []};
			_ -> {Buffer, Rest, Deferred}
		end
	end, {Buffer0, Rest0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	io:format(standard_error, "pos2 receives (~p)~n", [Received]),
	pos2(buffer_update([key2], [pos2], Received, NBuffer), NRest, NDeferred).


buffer(Buffer0, Rest0, Deferred0) ->
	io:format(standard_error, "buffer(~p,~p,~p)~n", [Buffer0, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Deferred}) -> 
		case E of
			{ {unified@0, _} = Version, #{pos2 := Spos2, pos1 := Spos1, key2 := Skey2, key1 := Skey1, {last, buffer} := LSbuffer} = Map} ->
				Curr = (case {fun_update_buffer(1,Spos1,Skey1,LSbuffer)} of {Vb1} -> (case {fun_update_buffer(2,Spos2,Skey2,Vb1)} of {Vb2} -> Vb2 end) end),
				lists:foreach(fun (V) -> 
					{board,'server@localhost'} ! {buffer, Curr, V},
					{buffer,'server@localhost'} ! {buffer, Curr, V}
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), []};
			_ -> {Buffer, Rest, Deferred}
		end
	end, {Buffer0, Rest0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	io:format(standard_error, "buffer receives (~p)~n", [Received]),
	buffer(buffer_update([pos2, pos1, key2, key1], [buffer], Received, NBuffer), NRest, NDeferred).


board(Buffer0, Rest0, Deferred0) ->
	io:format(standard_error, "board(~p,~p,~p)~n", [Buffer0, Rest0, Deferred0]),
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Deferred}) -> 
		case E of
			{ {unified@0, _} = Version, #{pos2 := Spos2, pos1 := Spos1, buffer := Sbuffer} = Map} ->
				Curr = {[Spos1,Spos2],Sbuffer},
				lists:foreach(fun (V) -> 
					out_node ! {board, Curr, V}
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), []};
			_ -> {Buffer, Rest, Deferred}
		end
	end, {Buffer0, Rest0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	io:format(standard_error, "board receives (~p)~n", [Received]),
	board(buffer_update([pos2, pos1, buffer], [], Received, NBuffer), NRest, NDeferred).

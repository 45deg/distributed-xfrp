-module(board).

-export([start/1]).
-export([out/2,out_node/1,in/1,wait/1]).
-export([fun_assoc_update/3,fun_movebykey/2,fun_update_buffer/4]).
-export([key1/1,key2/1]).
-export([pos1/4,pos2/4,buffer/4,board/4]).

-define(SORTBuffer, fun ({{K1, V1}, _}, {{K2, V2}, _}) -> if
	V1 == V2 -> K1 < K2;
	true -> V1 < V2
end end).
buffer_update(CurrentIds, LastIds, {Id, RValue, {RVId, RVersion}}, Buffer, Last) ->
	NBuffer = case lists:member(Id, CurrentIds) of
		true  -> maps:update_with({RVId, RVersion}, fun(M) -> M#{ Id => RValue } end, #{ Id => RValue }, Buffer);
		false -> Buffer
	end,
	NLast = case lists:member(Id, LastIds) of
		true  -> maps:update_with(Id, fun({Ver,_}) -> {Ver#{RVId => RVersion + 1}, RValue} end, Last);
		false -> Last
	end,
	{NBuffer, NLast}.
wait(Hosts) -> 
	case lists:all(fun (N) -> net_kernel:connect_node(N) end, Hosts) of
		true -> ok;
		false -> timer:sleep(1000), wait(Hosts)
	end.

fun_assoc_update(Vkey,Vvalue,Vlist) -> (case Vlist of [] -> [{Vkey,Vvalue}]; [{Vk,Vv}|Vls] -> (case (Vk == Vkey) of true -> [{Vkey,Vvalue}|Vls]; false -> [{Vk,Vv}|fun_assoc_update(Vkey,Vvalue,Vls)] end) end).

fun_movebykey(Vpos,Vkey) -> (case Vpos of {Vx,Vy} -> (case Vkey of 258 -> {(Vx + 1),Vy}; 259 -> {(case (Vx == 0) of true -> Vx; false -> (Vx - 1) end),Vy}; 260 -> {Vx,(case (Vy == 0) of true -> Vy; false -> (Vy - 1) end)}; 261 -> {Vx,(Vy + 1)}; _ -> {Vx,Vy} end) end).

fun_update_buffer(Vplayer,Vpos,Vkey,Vbuffer) -> (case ((32 =< Vkey) andalso (Vkey =< 126)) of true -> fun_assoc_update(Vpos,{Vplayer,Vkey},Vbuffer); false -> Vbuffer end).

start('client1@localhost') -> 
	register(pos1, spawn(?MODULE, pos1, [#{}, #{}, #{pos1=>{#{key1=>0},{0,0}}}, []])),
	register(key1, spawn(?MODULE, key1, [0])),
	wait(['server@localhost']),
	in('client1@localhost');
start('client2@localhost') -> 
	register(pos2, spawn(?MODULE, pos2, [#{}, #{}, #{pos2=>{#{key2=>0},{0,0}}}, []])),
	register(key2, spawn(?MODULE, key2, [0])),
	wait(['server@localhost']),
	in('client2@localhost');
start('server@localhost') -> 
	register(board, spawn(?MODULE, board, [#{}, #{}, #{}, []])),
	register(buffer, spawn(?MODULE, buffer, [#{}, #{}, #{buffer=>{#{key1=>0,key2=>0},[]}}, []])),
	register(out_node,spawn(?MODULE, out_node, ['server@localhost'])),
	wait([]),
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
		{buffer,'server@localhost'} ! {key1, Value, {key1, Version}},
		{pos1,'client1@localhost'} ! {key1, Value, {key1, Version}}
	end,
	key1(Version + 1).

key2(Version) ->
	receive
	Value ->
		{buffer,'server@localhost'} ! {key2, Value, {key2, Version}},
		{pos2,'client2@localhost'} ! {key2, Value, {key2, Version}}
	end,
	key2(Version + 1).

out_node('server@localhost') ->
	receive
		{board, Value, _} -> out(board, Value)
	end,
	out_node('server@localhost');
out_node(_) -> erlang:error(badarg).

pos1(Buffer0, Rest0, Last0, Deferred0) ->
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NLast, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Last, Deferred}) -> 
		case {E, Last} of
			{{ {key1, Vc} = Version, #{key1 := Skey1} = Map}, #{pos1 := {#{key1 := Vl}, LSpos1}}} when Vc == Vl ->
				Curr = fun_movebykey(LSpos1,Skey1),
				lists:foreach(fun (V) -> 
					{board,'server@localhost'} ! {pos1, Curr, V},
					{buffer,'server@localhost'} ! {pos1, Curr, V},
					{pos1,'client1@localhost'} ! {pos1, Curr, V}
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, []};
			_ -> {Buffer, Rest, Last, Deferred}
		end
	end, {Buffer0, Rest0, Last0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	{UBuffer, ULast} = buffer_update([key1], [pos1], Received, NBuffer, NLast),
	pos1(UBuffer, NRest, ULast, NDeferred).


pos2(Buffer0, Rest0, Last0, Deferred0) ->
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NLast, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Last, Deferred}) -> 
		case {E, Last} of
			{{ {key2, Vc} = Version, #{key2 := Skey2} = Map}, #{pos2 := {#{key2 := Vl}, LSpos2}}} when Vc == Vl ->
				Curr = fun_movebykey(LSpos2,Skey2),
				lists:foreach(fun (V) -> 
					{board,'server@localhost'} ! {pos2, Curr, V},
					{buffer,'server@localhost'} ! {pos2, Curr, V},
					{pos2,'client2@localhost'} ! {pos2, Curr, V}
				end, [Version|Deferred]),
				{maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, []};
			_ -> {Buffer, Rest, Last, Deferred}
		end
	end, {Buffer0, Rest0, Last0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	{UBuffer, ULast} = buffer_update([key2], [pos2], Received, NBuffer, NLast),
	pos2(UBuffer, NRest, ULast, NDeferred).


buffer(Buffer0, Rest0, Last0, Deferred0) ->
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NLast, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Last, Deferred}) -> 
		case {E, Last} of
			{{ {key1, Vc} = Version, #{pos1 := Spos1,key1 := Skey1} = Map}, #{buffer := {#{key1 := Vl}, LSbuffer}}} when Vc == Vl ->
				case {Rest,Last} of
					{#{pos2 := Spos2,key2 := Skey2},_} -> 
						Curr = (case {fun_update_buffer(1,Spos1,Skey1,LSbuffer)} of {Vb1} -> (case {fun_update_buffer(2,Spos2,Skey2,Vb1)} of {Vb2} -> Vb2 end) end),
						lists:foreach(fun (V) -> 
							{board,'server@localhost'} ! {buffer, Curr, V},
							{buffer,'server@localhost'} ! {buffer, Curr, V}
						end, [Version|Deferred]),
						{maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, []};
					_ -> {maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, [Version|Deferred]}
				end;
			{{ {key2, Vc} = Version, #{pos2 := Spos2,key2 := Skey2} = Map}, #{buffer := {#{key2 := Vl}, LSbuffer}}} when Vc == Vl ->
				case {Rest,Last} of
					{#{pos1 := Spos1,key1 := Skey1},_} -> 
						Curr = (case {fun_update_buffer(1,Spos1,Skey1,LSbuffer)} of {Vb1} -> (case {fun_update_buffer(2,Spos2,Skey2,Vb1)} of {Vb2} -> Vb2 end) end),
						lists:foreach(fun (V) -> 
							{board,'server@localhost'} ! {buffer, Curr, V},
							{buffer,'server@localhost'} ! {buffer, Curr, V}
						end, [Version|Deferred]),
						{maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, []};
					_ -> {maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, [Version|Deferred]}
				end;
			_ -> {Buffer, Rest, Last, Deferred}
		end
	end, {Buffer0, Rest0, Last0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	{UBuffer, ULast} = buffer_update([pos2, pos1, key2, key1], [buffer], Received, NBuffer, NLast),
	buffer(UBuffer, NRest, ULast, NDeferred).


board(Buffer0, Rest0, Last0, Deferred0) ->
	HL = lists:sort(?SORTBuffer, maps:to_list(Buffer0)),
	{NBuffer, NRest, NLast, NDeferred} = lists:foldl(fun (E, {Buffer, Rest, Last, Deferred}) -> 
		case {E, Last} of
			{{ {key1, _} = Version, #{pos1 := Spos1,buffer := Sbuffer} = Map}, _} ->
				case {Rest,Last} of
					{#{pos2 := Spos2},_} -> 
						Curr = {[Spos1,Spos2],Sbuffer},
						lists:foreach(fun (V) -> 
							out_node ! {board, Curr, V}
						end, [Version|Deferred]),
						{maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, []};
					_ -> {maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, [Version|Deferred]}
				end;
			{{ {key2, _} = Version, #{pos2 := Spos2,buffer := Sbuffer} = Map}, _} ->
				case {Rest,Last} of
					{#{pos1 := Spos1},_} -> 
						Curr = {[Spos1,Spos2],Sbuffer},
						lists:foreach(fun (V) -> 
							out_node ! {board, Curr, V}
						end, [Version|Deferred]),
						{maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, []};
					_ -> {maps:remove(Version, Buffer), maps:merge(Rest, Map), Last, [Version|Deferred]}
				end;
			_ -> {Buffer, Rest, Last, Deferred}
		end
	end, {Buffer0, Rest0, Last0, Deferred0}, HL),
	Received = receive {_,_,{_, _}} = M -> M end,
	{UBuffer, ULast} = buffer_update([pos2, pos1, buffer], [], Received, NBuffer, NLast),
	board(UBuffer, NRest, ULast, NDeferred).

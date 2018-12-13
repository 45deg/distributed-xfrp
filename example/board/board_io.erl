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
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
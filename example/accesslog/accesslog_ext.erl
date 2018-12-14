-include_lib("wx/include/wx.hrl").

getLocation(_) ->
  Countries = ["USA", "Canada", "Germany", "UK", "Spain", "France", "Russian"],
  timer:sleep(rand:uniform(1000)),
  lists:nth(rand:uniform(length(Countries)), Countries).

access_actor() ->
  timer:sleep(rand:uniform(2000)),
	access ! {erlang:system_time(),
            lists:flatten(io_lib:format("~p.~p.~p.~p", [rand:uniform(256), rand:uniform(256), rand:uniform(256), rand:uniform(256)]))},
  access_actor().

in('localhost') ->
  searchText ! "",
  spawn(fun () -> access_actor() end),
  on_message(gui_init());
in(_) -> erlang:error(badarg).

set_elements(Frame, Ctrl, List) ->
  wxFrame:freeze(Frame),
  wxListCtrl:deleteAllItems(Ctrl),
  lists:foldl(fun ({Timestamp, IP, Geo}, I) -> 
    Time = calendar:system_time_to_local_time(Timestamp, native),
    {_, {H, M, S}} = Time,
    TimeStr = io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]),
    Index = wxListCtrl:insertItem(Ctrl, I, TimeStr),
    wxListCtrl:setItem(Ctrl, Index, 1, IP),
    wxListCtrl:setItem(Ctrl, Index, 2, Geo),
    I+1
  end, 0, List),
  wxFrame:thaw(Frame).

on_update(Text) ->
  searchText ! Text.

on_message({Frame, ListCtrl}) ->
  receive
    {display, List} -> set_elements(Frame, ListCtrl, List);
    {count, Number} -> wxFrame:setStatusText(Frame, io_lib:format("~p messages arrived.~n", [Number]));
    _ -> ok
  end,
	on_message({Frame, ListCtrl}).

gui_init() ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,-1,"Accesslog",[{size, {600,800}}]),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),

  wxFrame:createStatusBar(Frame),
  wxFrame:setStatusText(Frame, "Quiet here."),

  TextCtrl = wxTextCtrl:new(Frame, ?wxID_ANY),
  wxFrame:connect(Frame, command_text_updated, [{callback, fun (_, Event) -> 
    on_update(wxCommandEvent:getString(Event))
  end}]),

  ListCtrl = wxListCtrl:new(Frame, [{size, {600,640}}, {style, ?wxLC_REPORT}]),
  C1 = wxListItem:new(),
  wxListItem:setId(C1, -1),
  wxListItem:setText(C1, "Time"),
  wxListItem:setWidth(C1, 200),
  wxListCtrl:insertColumn(ListCtrl, 0, C1),
  C2 = wxListItem:new(),
  wxListItem:setId(C2, -1),
  wxListItem:setText(C2, "IP"),
  wxListItem:setWidth(C2, 200),
  wxListCtrl:insertColumn(ListCtrl, 1, C2),
  C3 = wxListItem:new(),
  wxListItem:setId(C3, -1),
  wxListItem:setText(C3, "Location"),
  wxListItem:setWidth(C3, 200),
  wxListCtrl:insertColumn(ListCtrl, 2, C3),

  % set_elements(ListCtrl, [{ erlang:system_time(), "192.168.11.12", "Japan" }, { erlang:system_time(), "192.168.11.1", "Japan" }]),

  Label = wxStaticText:new(Frame, ?wxID_ANY, "Numbers: "),

  wxSizer:add(MainSizer, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}]),
  wxSizer:add(MainSizer, Label, [{flag, ?wxALL bor ?wxEXPAND}, {border, 5}]),
  wxSizer:add(MainSizer, ListCtrl, [{flag, ?wxALL bor ?wxEXPAND}]),
  
  wxFrame:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer, Frame),

  wxFrame:show(Frame),

  {Frame, ListCtrl}.
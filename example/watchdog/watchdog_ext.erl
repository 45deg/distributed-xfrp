-include_lib("wx/include/wx.hrl").

-define (CHOICES, [
  {"Very Low",5},
  {"Low",4},
  {"Middle",3},
  {"High",2},
  {"Very High",1}
]).

%%% MAIN

evaluateThreat(_) ->
  timer:sleep(rand:uniform(1500)),
  case rand:uniform(5) > 0 of
    true -> rand:uniform(5);
    false -> 0
  end.

sensor_actor() ->
  timer:sleep(1000),
	sensor ! {rand:uniform(100), erlang:system_time()},
  sensor_actor().

on_message({Sizer, Report, Label}) ->
  receive
    {alarm, true} -> set_status(Label, unsafe);
    {alarm, false} -> set_status(Label, safe);
    {report, Logs} -> set_text(Report, string:join(lists:map(fun ({ThreatLevel,Timestamp,Value}) -> 
      Time = calendar:system_time_to_local_time(Timestamp, native),
      {_, {H, M, S}} = Time,
      io_lib:format('[~2..0b:~2..0b:~2..0b] Level: ~p, Value: ~p', [H, M, S, ThreatLevel, Value])
    end, Logs), "\n"));
    _ -> ok
  end,
  wxSizer:layout(Sizer),
	on_message({Sizer, Report, Label}).

on_change(Level) ->
  threshold ! Level,
  io:format("Level: ~p~n", [Level]).

in('localhost') ->
  threshold ! 3,
  spawn(fun () -> sensor_actor() end),
  on_message(gui_init());
in(_) -> erlang:error(badarg).

%%% UI

set_text(Ctrl, Text) ->
  wxTextCtrl:setValue(Ctrl, Text).

set_status(Label, Status) ->
  case Status of
    safe ->
      wxStaticText:setBackgroundColour(Label, {0,255,0}),
      wxStaticText:setLabel(Label, "Safe");
    unsafe ->
      wxStaticText:setBackgroundColour(Label, {255,0,0}),
      wxStaticText:setLabel(Label, "Unsafe");
    _ -> ok
  end.

gui_init() ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx,-1,"Watchdogs",[{size, {600,800}}]),

  StatusLabel = wxStaticText:new(Frame, ?wxID_ANY, "Safe", [{style, ?wxALIGN_CENTRE}]),
  wxStaticText:setFont(StatusLabel, wxFont:new(36, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD)),
  wxStaticText:setForegroundColour(StatusLabel, {255,255,255}),
  set_status(StatusLabel, safe),

  ComboLabel = wxStaticText:new(Frame, ?wxID_ANY, "Security Level: "),
  ComboBox = wxComboBox:new(Frame, 1,
				[{style, ?wxCB_DROPDOWN bor ?wxCB_READONLY},
         {choices, lists:map(fun ({S, _}) -> S end, ?CHOICES)}]),
  wxComboBox:setValue(ComboBox, "Middle"),
  wxFrame:connect(Frame, command_combobox_selected, [{callback, fun (_, Event) -> 
    Key = wxCommandEvent:getString(Event),
    {_, Level} = lists:keyfind(Key, 1, ?CHOICES),
    on_change(Level)
  end}]),

  TextCtrlLabel = wxStaticText:new(Frame, ?wxID_ANY, "Report: "),
  TextCtrl = wxTextCtrl:new(Frame, ?wxID_ANY, [{style, ?wxTE_MULTILINE}, {size, {300, 500}}]),
  wxTextCtrl:setEditable(TextCtrl, false),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  ComboSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(MainSizer, StatusLabel,[{flag, ?wxALL bor ?wxEXPAND}]),

  wxSizer:add(ComboSizer, ComboLabel, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxSizer:add(ComboSizer, ComboBox, [{proportion,1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
  wxSizer:add(MainSizer, ComboSizer),

  wxSizer:add(MainSizer, TextCtrlLabel, [{flag, ?wxALL}, {border, 5}]),
  wxSizer:add(MainSizer, TextCtrl, [{flag, ?wxALL bor ?wxEXPAND}]),

  wxFrame:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer, Frame),

  wxFrame:show(Frame),

  {MainSizer, TextCtrl, StatusLabel}.

-module(fan_controller).
-export([main/0]).
-export([out/0, in/2]).
-export([tmp/3, hmd/3, di/3, fan/3, ho/3]).

main() ->
    Out = spawn(fun() -> out() end),
    Tmp = spawn(fun() -> tmp(0, {}, { null, null }) end),
    Hmd = spawn(fun() -> hmd(0, {}, { null, null }) end),
    Di = spawn(fun() -> di(0, { 0, 0 }, { null }) end),
    Fan = spawn(fun() -> fan(false, { 0, 0 }, { null, null }) end),
    Ho = spawn(fun() -> ho(false, { 0 }, { null }) end),
    % In = spawn(fun() -> in(Tmp, Hmd) end),
    Tmp ! {set, { Di, Ho }},
    Hmd ! {set, { Di, Ho }},
    Di ! { set, { Fan }},
    Fan ! { set, { Ho, Out }},
    Ho ! { set, { Fan }},
    in(Tmp, Hmd).

out() ->
    receive
        {fan, true} -> void;
            % io:fwrite("T\n");
        {fan, false} -> void
            % io:fwrite("F\n")
    end,
    out().

in(Tmp, Hmd) ->
    Tmp ! rand:uniform() * 5 + 20,
    Hmd ! rand:uniform() * 50 + 50,
    timer:sleep(1000),
    in(Tmp, Hmd).

tmp(Last, {}, { DiProc, HoProc }) ->
    receive 
        {set, Procs} -> tmp(Last, {}, Procs);
        Value -> 
            % io:fwrite("Tmp: ~f\n", [Value]),
            DiProc ! {tmp, Value},
            HoProc ! {tmp, Value},
            tmp(Value, {}, { DiProc, HoProc })
    end.

hmd(Last, {}, { DiProc, HoProc }) ->
    receive 
        {set, Procs} -> hmd(Last, {}, Procs);
        Value -> 
            % io:fwrite("hmd: ~f\n", [Value]),
            DiProc ! {hmd, Value},
            HoProc ! {hmd, Value},
            hmd(Value, {}, { DiProc, HoProc })
    end.

di(Last, { Tmp, Hmd }, { FanProc }) ->
    receive 
        {set, Procs} -> di(Last, { Tmp, Hmd }, Procs);
        {tmp, Value} when Value /= Tmp -> 
            Curr = 0.81 * Value + 0.01 * Hmd * (0.99 * Value - 14.3) + 46.3,
            FanProc ! {di, Curr},
            di(Curr, { Value, Hmd }, { FanProc });
        {hmd, Value} when Value /= Hmd -> 
            Curr = 0.81 * Hmd + 0.01 * Value * (0.99 * Hmd - 14.3) + 46.3,
            FanProc ! {di, Curr},
            di(Curr, { Tmp, Value }, { FanProc })
    end.

fan(Last, { Di, Ho }, { HoProc, OutProc }) ->
    receive 
        {set, Procs} -> fan(Last, { Di, Ho }, Procs);
        {di, Value} when Value /= Di -> 
            Curr = Value >= 75.0 + Ho,
            HoProc ! {fan, Curr},
            OutProc ! {fan, Curr},
            fan(Curr, { Value, Ho }, { HoProc, OutProc });
        {ho, Value} when Value /= Ho -> 
            Curr = Di >= 75.0 + Value,
            HoProc ! {fan, Curr},
            OutProc ! {fan, Curr},
            fan(Curr, { Di, Value }, { HoProc, OutProc })
    end.

ho(Last, { Fan }, { FanProc }) ->
    receive 
        {set, Procs} -> ho(Last, { Fan }, Procs);
        {fan, Value} when Value /= Fan -> 
            Curr = if Fan -> -0.5; true -> 0.5 end,
            FanProc ! { ho, Curr },
            ho(Curr, { Value }, { FanProc })
    end.

% module FanController  # module name
%in  tmp : Float,      # temperature sensor
%   hmd : Float       # humidity sensor
%out fan : Bool        # fan switch
%use Std               # standard library

%# discomfort (temperature-humidity) index
%node di =
%    0.81 * tmp + 0.01 * hmd * (0.99 * tmp - 14.3) + 46.3

%# fan switch
%node init[False] fan = di >= 75.0 + ho

%# hysteresis offset
%node ho = if fan@last then -0.5 else 0.5

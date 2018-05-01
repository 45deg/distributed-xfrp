#!/usr/bin/env escript

% Erlang AST Form Converter
% usage: ./ast.escript [-d] FILE

main(Args) -> main(false, Args).

main(_, ["-h" | _]) -> usage();
main(_, ["-d" | Args]) -> main(true, Args);
main(Decode, [FileName]) -> proc(Decode, FileName);
main(_, _) -> usage().

usage() ->
  io:format("./to_ast.escript [AST Source]"),
  halt(1).

eval(Expr)->
  {ok, Tokens, _} = erl_scan:string(Expr ++ "."),
  {ok,[Expression]} = erl_parse:parse_exprs(Tokens),
  {value, Ret, _} = erl_eval:expr(Expression, erl_eval:bindings(erl_eval:new_bindings())),
  Ret.

pp_forms([]) -> ok;
pp_forms([Form | Rest]) -> 
  io:format("~s~n", [ erl_pp:form(Form) ]),
  pp_forms(Rest).

proc(Decode, File) ->
  case Decode of 
    true ->
      {ok, Binary} = file:read_file(File),
      Contents = unicode:characters_to_list(Binary),
      Forms = eval(Contents),
      pp_forms(Forms);
    false ->
      {ok, Forms} = epp:parse_file(File, []),
      io:format("~p", [Forms])
  end.

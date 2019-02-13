#!/usr/bin/env escript
%%! -pa ebin -smp enable -sname erl_tester

main([FileName,PropName]) ->
  io:format("~p~n", [FileName]),
  io:format("~p~n", [PropName]),
  Forms = forms:read(FileName),
  APropName = list_to_atom(PropName),
  FiltPred = fun (Form) ->
    try erl_syntax:type(Form) of
      function ->
        CFunName = erl_syntax:concrete(erl_syntax:function_name(Form)),
        case CFunName of
          APropName -> true;
          _         -> false
        end;
      _ -> false
    catch
      _:_ -> false
    end
  end,
  {ok, Input}  = io:read(""),
  PropFuns = forms:filter(FiltPred, Forms),
  % Handle case when |PropFuns| > 1
  PropFun = lists:nth(1, PropFuns),
  % io:format("~p~n", [PropFuns]).
  PInput = testing:parse_input(Input, PropFun),
  io:format("~p~n", [PInput]).

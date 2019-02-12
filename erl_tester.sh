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
  PropFun = forms:filter(FiltPred, Forms),
  io:format("~p~n", [PropFun]).
  
  
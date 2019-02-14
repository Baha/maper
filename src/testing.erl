-module(testing).
-export([parse_input/2]).

parse_input(Input, PropFun) ->
  % check if PropFun expects composite
  PropClauses = erl_syntax:function_clauses(PropFun),
  ClauseBody = erl_syntax:clause_body(hd(PropClauses)),
  {call,_,{remote,_,{atom,_,ProperMod},{atom,_,ProperCall}},CallArgs} = hd(ClauseBody),
  PropFormat = lists:nth(1, CallArgs),
  io:format("~p~n", [CallArgs]),
  PInput =
    case is_composite(PropFormat) of
      true ->
        convert_input(Input);
      false ->
        Input
    end,
  PInput.

is_composite({cons,_,_,_}) -> true;
is_composite({tuple,_,_})  -> true;
is_composite(_) -> false.

convert_input(X) -> X.

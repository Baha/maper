-module(ord_insert_bug).
-export([ordered/1,insert/2]).

-include_lib("proper/include/proper.hrl").

ordered(L) ->
  case L of
    [A,B|T] -> A =< B andalso ordered([B|T]);
    _  -> true % smaller lists
  end.

insert(I,L) ->
  case L of
    [] -> [I];
    [X|Xs] when I =< X -> [X,I|Xs]; % it should be [I,X|Xs]
    [X|Xs] -> [X] ++ insert(I,Xs)
  end.

%===============================================================================
%% Specification of the insertion function (part of the insertion sort algoritm)
ordered_list() ->
  ?SUCHTHAT(L, non_empty(list(integer())), ordered(L)).

prop_tbt() ->
  ?FORALL({E,L}, { integer(), ordered_list() }, ordered(insert(E,L))).

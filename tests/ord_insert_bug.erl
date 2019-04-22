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

prop_insert() ->
  ?FORALL({E,L}, { integer(), ordered_list() }, ordered(insert(E,L))).

% Eshell V9.2  (abort with ^G)
% 1> c('tests/ord_insert_bug.erl').
% {ok,ord_insert_bug}
% 2> ord_insert_bug:insert(2,[1,2,5]).
% [1,2,2,5]                                   % OK
% 3> ord_insert_bug:insert(2,[1,3,5]).
% [1,3,2,5]                                   % BUG!
% 4> ord_insert_bug:ordered(ord_insert_bug:insert(2,[1,3,5])).
% false
% 5> ord_insert_bug:ordered(ord_insert_bug:insert(2,[1,2,5])).
% true

% Prolog tests:
% ?- use_module('src/eval.pl'), consult('src/proper_types.pl'), reconsult('tests/ord_insert_bug.pl').
% typeof(I,non_empty(list(integer))), eval(apply(var('ordered',1),[var('List')]),[('List',I)],lit(atom,true)).

-module(ord_insert_bug).
-export([insert/2,is_ordered/1]).

-include_lib("proper/include/proper.hrl").

is_ordered([A,B|T]) ->
  A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % smaller lists
  true.

insert(I,[]) ->
  [I];
insert(I,[X|Xs]) when I =< X ->
  [X,I|Xs]; % it should be [I,X|Xs]
insert(I,[X|Xs]) ->
  [X] ++ insert(I,Xs).

%===============================================================================
%% Specification of the insertion function (part of the insertion sort algoritm)
ordered_list() ->
  ?SUCHTHAT(L, non_empty(list(integer())), is_ordered(L)).

prop_ordered_insert() ->
  ?FORALL( {E,L}, { integer(), ordered_list() },
    is_ordered(insert(E,L))
  ).

% Eshell V9.2  (abort with ^G)
% 1> c('tests/ord_insert_bug.erl').
% {ok,ord_insert_bug}
% 2> ord_insert_bug:insert(2,[1,2,5]).
% [1,2,2,5]                                   % OK
% 3> ord_insert_bug:insert(2,[1,3,5]).
% [1,3,2,5]                                   % BUG!
% 4> ord_insert_bug:is_ordered(ord_insert_bug:insert(2,[1,3,5])).
% false
% 5> ord_insert_bug:is_ordered(ord_insert_bug:insert(2,[1,2,5])).
% true

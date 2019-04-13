-module(ord_insert_ok).
-export([ord_insert/2,is_ordered/1]).

-include_lib("proper/include/proper.hrl").

is_ordered([A,B|T]) ->
  A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % smaller lists
  true.

ord_insert(I,[]) ->
  [I];
ord_insert(I,[X|Xs]) when I =< X ->
  [I,X|Xs];
ord_insert(I,[X|Xs]) ->
  [X] ++ ord_insert(I,Xs).

%==============================================================================
%% Specification of the insertion sort
ordered_list() ->
  ?SUCHTHAT(List, non_empty(list(integer())), is_ordered(List)).

prop_ordered_insert() ->
  ?FORALL( {Elem,List}, { integer(), ordered_list() },
    is_ordered(ord_insert(Elem,List))
  ).

% Eshell V9.2  (abort with ^G)
% 1> c('tests/ord_insert_ok.erl').
% {ok,ord_insert_ok}
% 2> ord_insert_ok:ord_insert(2,[1,2,5]).
% [1,2,2,5]
% 3> ord_insert_ok:ord_insert(2,[1,3,5]).
% [1,2,3,5]
% 4> ord_insert_ok:is_ordered(ord_insert_ok:ord_insert(2,[1,2,5])).
% true
% 5> ord_insert_ok:is_ordered(ord_insert_ok:ord_insert(2,[1,3,5])).
% true

-module(ord_insert_ok).
-export([ord_insert/2,is_ordered/1]).

-include_lib("proper/include/proper.hrl").

is_ordered([A,B|T]) ->
    A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % smaller lists
    true.

ord_insert(I,[]) ->
    [I];
ord_insert(I,[X|Xs]) when I < X ->
    [I,X|Xs];
ord_insert(I,[X|Xs]) ->
    [X] ++ ord_insert(I,Xs).


ordered_list() ->
    ?SUCHTHAT(List, non_empty(list(integer())), is_ordered(List)).

prop_ordered_insert() ->
    ?FORALL({Elem,List}, {integer(),ordered_list()},
        begin
            is_ordered(ord_insert(Elem,List))
        end).

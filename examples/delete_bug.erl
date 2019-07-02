-module(delete_bug).
-export([delete/2,list_by_del/1,delete_rpt/2]).

%% Properties, i.e. 0-arity functions whose name begins with "prop_" are
%% automatically exported.

-include_lib("proper/include/proper.hrl").

%==============================================================================
%% A faulty implementation of lists:delete/2, it will fail if the list contains
%% duplicates:

delete_rpt(D, L) ->
  case D of
    [] -> L;
    [X|Xs] -> delete_rpt(Xs, delete(X, L, []))
  end.

delete(X, L) ->
    delete(X, L, []).

delete(_, [], Acc) ->
    lists:reverse(Acc);
delete(X, [X|Rest], Acc) ->
    lists:reverse(Acc) ++ Rest;
delete(X, [Y|Rest], Acc) ->
    delete(X, Rest, [Y|Acc]).

%==============================================================================
%% Properties for delete

prop_delete1() ->
    ?FORALL({X,L}, {integer(),list(integer())},
      not lists:member(X, delete(X,L))).

list_and_elem1() ->
    ?SUCHTHAT({X,L}, {integer(),list(integer())},
      lists:member(X,L)).

prop_delete2() ->
    ?FORALL([X,L], list_and_elem1(),
      not lists:member(X, delete(X,L))).

list_by_del([[],[],[]]) -> true;
list_by_del([[X|Xs],[X|Ys],Zs]) ->
    list_by_del([Xs,Ys,Zs]);
list_by_del([[X|Xs],Ys,[X|Zs]]) ->
    list_by_del([Xs,Ys,Zs]);
list_by_del(_) -> false.

lists_by_del() ->
    ?SUCHTHAT(L, list(list(integer())),
      list_by_del(L) ).

prop_tbt() ->
    ?FORALL(L, lists_by_del(), begin [L,D,R] = L, R =:= delete_rpt(D,L) end).

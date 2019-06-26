-module(delete_bug).
-export([delete/2]).

%% Properties, i.e. 0-arity functions whose name begins with "prop_" are
%% automatically exported.

-include_lib("proper/include/proper.hrl").

%==============================================================================

%% A faulty implementation of lists:delete/2, it will fail if the list contains
%% duplicates:

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

-module(delete_bug).
-export([delete/2]).

%% Properties, i.e. 0-arity functions whose name begins with "prop_" are
%% automatically exported.

-include_lib("proper/include/proper.hrl").
% -include_lib("eunit/include/eunit.hrl").

%% PropEr's builtin generators are automatically imported by the PropEr header.

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

%% Unit tests for delete
%% Run them using eunit:test(simple_props_demo).

% delete_test_() ->
%     [?_assert(delete(X,L1) =:= L2) || {X,L1,L2} <- testcases()].

% testcases() ->
%     [{1,[],[]},
%      {1,[1],[]},
%      {2,[1],[1]},
%      {2,[2,1],[1]},
%      {1,[1,2,3],[2,3]},
%      {2,[1,2,3],[1,3]},
%      {3,[1,2,3],[1,2]},
%      {4,[1,2,3],[1,2,3]},
%      {100,lists:seq(1,200),lists:seq(1,99) ++ lists:seq(101,200)}].

%==============================================================================

%% Properties for delete
%% Test each property with proper:quickcheck(simple_props_demo:property()), or
%% all of them at once with proper:module(simple_props_demo).

prop_delete() ->
    ?FORALL({X,L}, {integer(),list(integer())},
	    not lists:member(X, delete(X,L))).

prop_delete_more() ->
    numtests(1000,
	     ?FORALL({X,L}, {integer(),list(integer())},
		     not lists:member(X, delete(X,L)))).

prop_delete_with_stats() ->
    ?FORALL({X,L}, {integer(),list(integer())},
	    collect(lists:member(X,L),
		    not lists:member(X, delete(X,L)))).

prop_delete_only_interesting1() ->
    ?FORALL({X,L}, {integer(),list(integer())},
	    ?IMPLIES(lists:member(X,L),
		     not lists:member(X, delete(X,L)))).

list_and_elem1() ->
    ?SUCHTHAT({X,L}, {integer(),list(integer())},
	      lists:member(X,L)).

prop_delete_only_interesting2() ->
    ?FORALL([X,L], list_and_elem1(),
	    not lists:member(X, delete(X,L))).

list_and_elem2() ->
    ?LET(L, non_empty(list(integer())), {oneof(L),L}).

prop_delete_only_interesting3() ->
    ?FORALL({X,L}, list_and_elem2(),
	    not lists:member(X, delete(X,L))).


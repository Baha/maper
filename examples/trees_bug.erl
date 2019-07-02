-module(trees_bug).
-export([member/2, join/2, delete/2, tree_sum/1, preorder/1]).

%% NOTE: Must compile with debug_info enabled.

-include_lib("proper/include/proper.hrl").

%==============================================================================

%% API for trees

-type tree(T) :: 'leaf' | {'node',T,tree(T),tree(T)}.

-spec member(T, tree(T)) -> boolean().
member(_, leaf) ->
    false;
member(X, {node,X,_,_}) ->
    true;
member(X, {node,_,L,R}) ->
    member(X, L) orelse member(X, R).

-spec join(tree(T), tree(T)) -> tree(T).
join(leaf, T2) ->
    T2;
join({node,X1,L1,R1}, T2) ->
    {node,X1,join(L1, R1),T2}.

%% Bad implementation of delete: doesn't delete any occurence of X that is
%% nested under some other node containing X.
-spec delete(T, tree(T)) -> tree(T).
delete(_, leaf) ->
    leaf;
delete(X, {node,X,L,R}) ->
    join(L, R);
delete(X, {node,Y,L,R}) ->
    {node,Y,delete(X, L),delete(X, R)}.

-spec tree_sum(tree(number())) -> number().
tree_sum(leaf) ->
    0;
tree_sum({node,X,L,R}) ->
    tree_sum(L) + tree_sum(R) + X.

-spec preorder(tree(T)) -> [T].
preorder(leaf) ->
    [];
preorder({node,X,L,R}) ->
    [X] ++ preorder(L) ++ preorder(R).

%==============================================================================

%% Properties of the tree API

prop_tbt() ->
    ?FORALL({X,T}, {integer(),tree(integer())},
	    not member(X, delete(X, T))).

prop_sum() ->
    ?FORALL(T, tree(integer()),
	    lists:sum(preorder(T)) =:= tree_sum(T)).

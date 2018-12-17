-module(mytrees).
-export([tree1/1, tree2/1, tree3/1, tree4/1, tree5/1, tree/1]).
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

prop_delete() ->
    ?FORALL({X,T}, {integer(),tree(integer())},
	    not member(X, delete(X, T))).

%% Example of a PBT strategy: finding two distinct computations that should
%% result in the same value.
prop_sum() ->
    ?FORALL(T, tree(integer()),
	    lists:sum(preorder(T)) =:= tree_sum(T)).

%==============================================================================

%% Attempts at writing a generator for trees:

tree1(G) ->
    union(['leaf',
	   {'node',G,tree1(G),tree1(G)}
	  ]).

%% Erlang is eager: we need to enforce lazy evaluation to avoid infinite
%% recursion.

tree2(G) ->
    union(['leaf',
	   ?LAZY({'node',G,tree2(G),tree2(G)})
	  ]).

%% Generation might not terminate: we need to introduce a bound on the number
%% of recursive calls (and thus the size of the produced term), by handling the
%% 'size' parameter manually.

%% The base case is delegated to the 0-size clause.
%% All non-recursive cases are replaced by fallbacks to that clause.

tree3(G) ->
    ?SIZED(S, tree3(S, G)).

tree3(0, _) ->
    leaf;
tree3(S, G) ->
    union([tree3(0, G),
	   ?LAZY({'node',G,tree3(S-1, G),tree3(S-1, G)})
	  ]).

%% 50% of the time, the tree is empty: we should set the weights in the union
%% to ensure a satisfactory average size of produced instances.

tree4(G) ->
    ?SIZED(S, tree4(S, G)).

tree4(0, _) ->
    'leaf';
tree4(S, G) ->
    frequency([{1, tree4(0, G)},
	       {9, ?LAZY({'node',G,tree4(S-1, G),tree4(S-1, G)})}
	      ]).

%% The trees grow too fast: we should distribute the size evenly to all
%% subtrees.

tree5(G) ->
    ?SIZED(S, tree5(S, G)).

tree5(0, _) ->
    'leaf';
tree5(S, G) ->
    frequency([{1, tree5(0, G)},
	       {9, ?LAZY({'node',G,tree5(S div 2, G),tree5(S div 2, G)})}
	      ]).

%% Finally, we set up a more efficient shrinking strategy: pick each of the
%% subtrees in place of the tree that fails the property.

tree(G) ->
    ?SIZED(S, tree(S, G)).

tree(0, _) ->
    'leaf';
tree(S, G) ->
    frequency([{1, tree(0, G)},
	       {9, ?LAZY(?LETSHRINK([L,R],
				    [tree(S div 2, G),tree(S div 2, G)],
				    {'node',G,L,R}
				   ))}
	      ]).

%% Try this out using:
%% T2 = mytrees:tree(proper_types:integer()).
%% f(I), {ok,I} = proper_gen:pick(T2), io:format("~p~n",[I]).

%% Comment out the generator and recompile to use the type instead.
%% Try this out using:
%% {ok,T1} = proper_typeserver:demo_translate_type(mytrees, "tree(integer())").
%% f(I), {ok,I} = proper_gen:pick(T1), io:format("~p~n",[I]).

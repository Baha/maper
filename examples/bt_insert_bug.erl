-module(bt_insert_bug).
-export([height/1,left_rotate/1,right_rotate/1,bt/1,bt_insert/2]).

-include_lib("proper/include/proper.hrl").

%==============================================================================
%% API for bt
% bt tree is a self-balancing binary  tree where the difference between
% heights of left and right subtrees cannot be more than one for all nodes.
% (ref. https://www.geeksforgeeks.org/bt-tree-set-1-insertion/)
% (ref. https://www.tutorialspoint.com/data_structures_algorithms/bt_algorithm.htm)

-type tree(T) :: 'leaf' | {'node', tree(T), T, tree(T)}.

%% height(Tree) returns the height of Tree
-spec height(tree(any())) -> non_neg_integer().
height(T) ->
  case T of
    leaf -> 0;
    {node, Left, _Value, Right} ->
      max(height(Left),height(Right)) + 1
end.

% bt(Tree) returns true if Tree is a balanced binary  tree
-spec bt(tree(any())) -> boolean().
bt(T) ->
  case T of
    % 1. a leaf is a balanced binary  tree
    leaf -> true;
    % 2. a node is a balanced binary  tree if
    {node, Left, _ ,Right} ->
      % 2.1 the balance factor of a node (the difference between the
      % heights of the left and right subtrees) belongs to {-1,0,1}
      BalanceF = height(Left) - height(Right),
      BalanceF >= -1 andalso BalanceF =< 1 andalso
      % 2.4 the Left and the Right subtrees are balanced binary  trees
      bt(Left) andalso bt(Right);
    % returns false if neither 1 nor 2 hold or the input is not a tree
    _ -> false
  end.


%% bt_insert(Elem,Tree) adds Elem to Tree.
-spec bt_insert(T,tree(T)) -> tree(T).
bt_insert(E,T) ->
  case T of
    {node, L,V,R} ->
        re_balance({node, bt_insert(E,L),V,R});
    leaf ->
      {node, leaf,E,leaf}
  end.

%% re_balance(Tree) makes Tree balanced
-spec re_balance(tree(T)) -> tree(T).
re_balance(T) ->
  {node, L, _ ,R} = T,
  case height(L) - height(R) of
    % Left unbalanced
    2 -> right_rotate(T);
    % Right unbalanced
   -2 -> left_rotate(T);
    % Balanced
    _ -> T
  end.

%% right_rotate(Tree) creates a new tree rooted with the left node of Tree
-spec right_rotate(tree(T)) -> tree(T).
right_rotate({node, {node, LL,LV,LR},V,R}) -> {node, LL,LV,{node, LR,V,R}}.

%% left_rotate(Tree) creates a new tree rooted with the right node of Tree
-spec left_rotate(tree(T)) -> tree(T).
left_rotate({node, L,V,{node, RL,RV,RR}}) -> {node, {node, L,V,RL},RV,RR}.

%==============================================================================
%% Specification for bt trees API
% bt: balanced binary serach tree
bt() ->
  ?SUCHTHAT(Tree, tree(integer()), bt(Tree)).
% adding an element Elem to any bt tree, produces an bt tree
prop_tbt() ->
  ?FORALL({Elem,Tree}, {integer(),bt()}, bt(bt_insert(Elem,Tree))).

tree(G) ->
  union([
    'leaf',
    {'node',G,tree(G),tree(G)}
  ]).

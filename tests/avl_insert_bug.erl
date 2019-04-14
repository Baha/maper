-module(avl_insert_bug).
-export([height/1,bbst/1,left_rotate/1,right_rotate/1,insert/2]).

-include_lib("proper/include/proper.hrl").

%==============================================================================
%% API for AVL
% AVL tree is a self-balancing binary search tree where the difference between
% heights of left and right subtrees cannot be more than one for all nodes.
% (ref. https://www.geeksforgeeks.org/avl-tree-set-1-insertion/)
% (ref. https://www.tutorialspoint.com/data_structures_algorithms/avl_tree_algorithm.htm)

-type tree(T) :: 'leaf' | {'node', tree(T), T, tree(T)}.

%% height(Tree) returns the height of Tree
-spec height(tree(any())) -> non_neg_integer().
height(leaf) -> 0;
height({node, Left, _Value, Right}) ->
  max(height(Left),height(Right)) + 1.

% bbst(Tree) returns true if Tree is a balanced binary search tree, that is,
-spec bbst(tree(any())) -> boolean().
% 1. a leaf is a balanced binary search tree
bbst(leaf) -> true;
% 2. a node is a balanced binary search tree if
bbst({node, Left,Value,Right}) ->
  % 2.1 the balance factor of a node (the difference between the height of the
  % left subtree and the height of the right subtree) belongs to {-1,0,1}, and
  BalanceF = height(Left) - height(Right),
  BalanceF >= -1 andalso BalanceF =< 1 andalso
  % 2.2 the values of the nodes in the Left subtree are less than Value
  all_lt(Left,Value) andalso
  % 2.3 the values of the nodes in the Right subtree are greater than Value
  all_gt(Right,Value) andalso
  % 2.4 its subtrees are balanced binary search trees.
  bbst(Left) andalso bbst(Right);
% returns false if neither 1 nor 2 hold or the input is not a tree
bbst(_) -> false.

%% all_lt(Tree,Value) is true if the value of each node of Tree is
% less than Value
-spec all_lt(tree(any()),any()) -> boolean().
all_lt(leaf, _V) -> true;
all_lt({node, L,X,R},V) -> X < V andalso all_lt(L,V) andalso all_lt(R,V);
all_lt(_Tree, _V) -> false.

%% all_gt(Tree,Value) is true if the value of each node of Tree is
% greater than Value
-spec all_gt(tree(any()),any()) -> boolean().
all_gt(leaf, _V) -> true;
all_gt({node, L,X,R},V) -> X > V andalso all_gt(L,V) andalso all_gt(R,V);
all_gt(_Tree, _V) -> false.

%% insert(Elem,Tree) adds Elem to Tree.
% This buggy implementation, given any AVL tree, might not return an AVL tree.
-spec insert(T,tree(T)) -> tree(T).
insert(E,{node, L,V,R}) when E < V ->
  re_balance(E,{node, insert(E,L),V,R});
insert(E,{node, L,V,R}) when E > V ->
  re_balance(E,{node, L,V,insert(E,R)});
insert(E,{node, L,V,R}) when E == V ->
  {node, L,V,R};
insert(E,leaf) ->
  {node, leaf,E,leaf}.

%% re_balance(Elem,Tree) makes Tree balanced
-spec re_balance(T,tree(T)) -> tree(T).
re_balance(E,Root) ->
  {node, L,V,R} = Root,
  case height(L) - height(R) of
    % Left unbalanced
    2 -> {node, _,LV,_} = L,
      if
        E > LV -> right_rotate(Root); % > should be <
        E < LV -> right_rotate({node, left_rotate(L),V,R}) % < should be >
      end;
    % Right unbalanced
   -2 -> {node, _,RV,_} = R,
      if
        E < RV -> left_rotate(Root); % < should be >
        E > RV -> left_rotate({node, L,V,right_rotate(R)}) % > should be <
      end;
    % Balanced
    _ -> Root
  end.

%% right_rotate(Tree) creates a new tree rooted with the left node of Tree
-spec right_rotate(tree(T)) -> tree(T).
right_rotate({node, {node, LL,LV,LR},V,R}) -> {node, LL,LV,{node, LR,V,R}}.

%% left_rotate(Tree) creates a new tree rooted with the right node of Tree
-spec left_rotate(tree(T)) -> tree(T).
left_rotate({node, L,V,{node, RL,RV,RR}}) -> {node, {node, L,V,RL},RV,RR}.

%==============================================================================
%% Specification for AVL trees API
% avl_tree: balanced binary serach tree
avl_tree() ->
  ?SUCHTHAT(Tree, tree(integer()), bbst(Tree)).
% adding an element Elem to any AVL tree, produces an AVL tree
prop_insertion() ->
  ?FORALL({Elem,Tree}, {integer(),avl_tree()}, bbst(insert(Elem,Tree))).

tree(G) ->
  union([
    'leaf',
    {'node',G,tree(G),tree(G)}
  ]).

%
% avl_insert_bug:bbst(avl_insert_bug:insert(57,{node,{node,leaf,-24,{node,leaf,72,leaf}},82,{node,{node,leaf,91,leaf},94,{node,leaf,95,{node,leaf,100,leaf}}}})).

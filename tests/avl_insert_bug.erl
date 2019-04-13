-module(avl_insert_bug).
-export([height/1,balanced/1,left_rotate/1,right_rotate/1,insert/2]).

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

% balanced(Tree) returns true if Tree is balanced, that is,
-spec balanced(tree(any())) -> boolean().
% 1. a leaf is balanced
balanced(leaf) -> true;
% 2. a node is balanced if
balanced({node, Left, _Value, Right}) ->
  % 2.1 the balance factor of a node (the difference between the height of the
  % left subtree and the height of the right subtree) belongs to {-1,0,1}, and
  BalanceF = height(Left) - height(Right),
  BalanceF >= -1 andalso BalanceF =< 1  andalso
  % 2.2 its subtrees are balanced.
  balanced(Left) andalso balanced(Right);
% returns false if neither 1 nor 2 hold or the input is not a tree
balanced(_) -> false.

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
        E > RV -> left_rotate(Root);
        E < RV -> left_rotate({node, L,V,right_rotate(R)})
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
  ?SUCHTHAT(Tree, tree(integer()), balanced(Tree)).
% adding an element Elem to any AVL tree, produces an AVL tree
prop_insertion() ->
  ?FORALL({Elem,Tree}, {integer(),avl_tree()}, balanced(insert(Elem,Tree))).

tree(G) ->
  union([
    'leaf',
    {'node',G,tree(G),tree(G)}
  ]).

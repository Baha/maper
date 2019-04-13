-module(avl_insert_ok).
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
%% Erlang test cases (0--4 from https://www.geeksforgeeks.org/avl-tree-set-1-insertion/):
% 1> c('tests/avl_insert_ok.erl').
% {ok,avl_insert_ok}
% 0) requires no rotation
% avl_insert_ok:insert(14,{node,{node,{node,{node,leaf,4,leaf},5,{node,leaf,6,leaf}},10,{node,leaf,11,leaf}},13,{node,leaf,15,{node,leaf,16,leaf}}}).
% 1) LL (requires right rotation)
% avl_insert_ok:insert(3,{node,{node,{node,{node,leaf,4,leaf},5,{node,leaf,8,leaf}},10,{node,leaf,11,leaf}},13,{node,leaf,15,{node,leaf,16,leaf}}}).
% 2) RR (requires left rotation)
% avl_insert_ok:insert(45,{node,{node,leaf,5,leaf},30,{node,{node,leaf,32,leaf},35,{node,leaf,40,leaf}}}).
% 3) LR (requires rigth and left rotation)
% avl_insert_ok:insert(7,{node,{node,{node,{node,leaf,4,leaf},5,{node,leaf,6,leaf}},10,{node,leaf,11,leaf}},13,{node,leaf,15,{node,leaf,16,leaf}}}).
% 4) RL (requires left and rigth rotation)
% avl_insert_ok:insert(15,{node,{node,{node,leaf,1,leaf},2,{node,{node,leaf,3,leaf},4,leaf}},5,{node,{node,leaf,6,leaf},7,{node,leaf,9,{node,leaf,16,leaf}}}})
% 5) duplicated value: no insertion
% avl_insert_ok:insert(10,{node,{node,{node,{node,leaf,4,leaf},5,{node,leaf,8,leaf}},10,{node,leaf,11,leaf}},13,{node,leaf,15,{node,leaf,16,leaf}}}).

%% re_balance(Elem,Tree) makes Tree balanced
-spec re_balance(T,tree(T)) -> tree(T).
re_balance(E,Root) ->
  {node, L,V,R} = Root,
  case height(L) - height(R) of
    % Left unbalanced
    2 -> {node, _,LV,_} = L,
      if
        E < LV -> right_rotate(Root);
        E > LV -> right_rotate({node, left_rotate(L),V,R})
      end;
    % Right unbalanced
   -2 -> {node, _,RV,_} = R,
      if
        E > RV -> left_rotate(Root);
        E < RV -> left_rotate({node, L,V,right_rotate(R)})
      end;
    % Balanced
    B when B >= -1 andalso B =< 1 -> Root
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

%% *** TESTING Coroutining ***
% $ ./erl2pl.sh tests/avl_insert_ok.erl
% swipl
% ?- use_module('src/eval.pl'), consult('src/proper_types.pl'), reconsult('tests/avl_insert_ok.pl').
% ?- set_config(max_size(10)).
%--- testing height/1 ----------------------------------------------------------
%-- generate & test: (typeof;eval) ---------------------------------------------
% ?- time(findall(H,( H #=< 5, typeof(Tree,tree(integer)), eval(apply(var('height',1),[var('Tree')]),[('Tree',Tree)],lit(int,H)) ),Hs)), length(Hs,N).
% % 783,292,889 inferences, 135.791 CPU in 135.791 seconds (100% CPU, 5768386 Lips)
% Hs = [1, 2, 3, 4, 5, 5, 5, 4, 4|...],
% N = 3673.
%-- let the interpreter guiding the type generator (eval;typeof) ---------------
% ?- time(findall(H,( H #=< 5, eval(apply(var('height',1),[var('Tree')]),[('Tree',Tree)],lit(int,H)), typeof(Tree,tree(integer)) ),Hs)), length(Hs,N).
% % 160,458,536 inferences, 24.878 CPU in 24.879 seconds (100% CPU, 6449837 Lips)
% Hs = [5, 4, 5, 5, 5, 5, 5, 3, 5|...],
% N = 3673.
%--- testing gen_avl_tree/1 ----------------------------------------------------
% ?- time(gen_avl_tree(Tree)).
% % 38,986 inferences, 0.015 CPU in 0.015 seconds (100% CPU, 2654816 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _9764), lit(atom, leaf)]),
% _9764 in inf..sup ;
% % 10,858 inferences, 0.007 CPU in 0.007 seconds (100% CPU, 1483669 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _15736), tuple([lit(atom, node), lit(atom, leaf), lit(..., ...)|...])]),
% _15736 in inf..sup,
% _15782 in inf..sup ;
% % 250,136,982 inferences, 40.507 CPU in 40.507 seconds (100% CPU, 6175117 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _51482), lit(atom, leaf)]), lit(int, _51506), lit(atom, leaf)]),
% _51482 in inf..sup,
% _51506 in inf..sup
% * NOTE that we waited a long time to get the third answer.
% Using the "Interpreter First" approach, that is:
% gen_avl_tree_interpreterFirst((Tree)) :-
%   eval(
%     apply(var('balanced',1),[var('Tree')]),
%     [('Tree',Tree)],
%     lit(atom,true)
%   ),
%   typeof(Tree,tree(integer)).
%%
% ?- time(gen_avl_tree_interpreterFirst(Tree)).
% % 12,241 inferences, 0.007 CPU in 0.007 seconds (100% CPU, 1802429 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _47196), lit(atom, leaf)]),
% _47196 in inf..sup ;
% % 14,694 inferences, 0.013 CPU in 0.013 seconds (100% CPU, 1102986 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _32814), tuple([lit(atom, node), lit(atom, leaf), lit(..., ...)|...])]),
% _32814 in inf..sup,
% _32860 in inf..sup ;
% % 239,969 inferences, 0.058 CPU in 0.058 seconds (100% CPU, 4149272 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _44010), tuple([...|...])]), lit(int, _44080), tuple([lit(atom, node), lit(atom, leaf), lit(..., ...)|...])]),
% _44010 in inf..sup,
% _44056 in inf..sup,
% _44080 in inf..sup,
% _44126 in inf..sup,
% _44172 in inf..sup ;
% % 90,360 inferences, 0.030 CPU in 0.030 seconds (99% CPU, 3050153 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _126916), tuple([...|...])]), lit(int, _126986), tuple([lit(atom, node), tuple([...|...]), lit(..., ...)|...])]),
% _126916 in inf..sup,
% _126962 in inf..sup,
% _126986 in inf..sup,
% _127054 in inf..sup,
% _127078 in inf..sup,
% _127124 in inf..sup,
% _127170 in inf..sup ;
% % 128,012 inferences, 0.042 CPU in 0.042 seconds (99% CPU, 3059824 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _109062), tuple([...|...])]), lit(int, _109132), tuple([lit(atom, node), tuple([...|...]), lit(..., ...)|...])]),
% _109062 in inf..sup,
% _109108 in inf..sup,
% _109132 in inf..sup,
% _109200 in inf..sup,
% _109224 in inf..sup,
% _109292 in inf..sup,
% _109316 in inf..sup ;
% % 21,982 inferences, 0.023 CPU in 0.023 seconds (99% CPU, 955111 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _96724), tuple([...|...])]), lit(int, _96794), tuple([lit(atom, node), tuple([...|...]), lit(..., ...)|...])]),
% _96724 in inf..sup,
% _96770 in inf..sup,
% _96794 in inf..sup,
% _96862 in inf..sup,
% _96886 in inf..sup,
% _96954 in inf..sup,
% _96978 in inf..sup,
% _97024 in inf..sup

-module(avl_insert_ok).
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
  ?SUCHTHAT(Tree, tree(integer()), bbst(Tree)).
% adding an element Elem to any AVL tree, produces an AVL tree
prop_insertion() ->
  ?FORALL({Elem,Tree}, {integer(),avl_tree()}, bbst(insert(Elem,Tree))).

tree(G) ->
  union([
    'leaf',
    {'node',G,tree(G),tree(G)}
  ]).

% avl_insert_ok:bbst(avl_insert_ok:insert(57,{node,{node,leaf,-24,{node,leaf,72,leaf}},82,{node,{node,leaf,91,leaf},94,{node,leaf,95,{node,leaf,100,leaf}}}})).

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
%%-- testing gen_avl_tree/1 ----------------------------------------------------
% ?- time(gen_avl_tree(Tree)).
% % 407 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 641034 Lips)
% Tree = lit(atom, leaf) ;
% % 24,853 inferences, 0.012 CPU in 0.012 seconds (100% CPU, 2057818 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _15202), lit(atom, leaf)]),
% _15202 in inf..sup ;
% % 15,653 inferences, 0.015 CPU in 0.015 seconds (100% CPU, 1021072 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _27656), tuple([lit(atom, node), lit(atom, leaf), lit(int, _27702), lit(atom, leaf)])]),
% _27656#=<_27702+ -1 ;
% % 645,348 inferences, 0.180 CPU in 0.180 seconds (100% CPU, 3586377 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _128206), tuple([lit(atom, node), lit(atom, leaf), lit(int, _128252), lit(atom, leaf)])]), lit(int, _128276), tuple([lit(atom, node), lit(atom, leaf), lit(int, _128322), tuple([lit(atom, node), lit(atom, leaf), lit(int, _128368), lit(atom, leaf)])])]),
% _128206#=<_128276+ -1,
% _128206#=<_128252+ -1,
% _128276#=<_128368+ -1,
% _128276#=<_128322+ -1,
% _128252#=<_128276+ -1,
% _128322#=<_128368+ -1 ;
% % 122,158 inferences, 0.059 CPU in 0.059 seconds (100% CPU, 2067943 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _115858), tuple([lit(atom, node), lit(atom, leaf), lit(int, _115904), lit(atom, leaf)])]), lit(int, _115928), tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _115996), lit(atom, leaf)]), lit(int, _116020), tuple([lit(atom, node), lit(atom, leaf), lit(int, _116066), tuple([lit(atom, node), lit(atom, leaf), lit(int, _116112), lit(atom, leaf)])])])]),
% _115858#=<_115928+ -1,
% _115858#=<_115904+ -1,
% _115928#=<_116112+ -1,
% _115928#=<_116066+ -1,
% _115928#=<_116020+ -1,
% _115928#=<_115996+ -1,
% _115904#=<_115928+ -1,
% _116066#=<_116112+ -1,
% _116020#=<_116112+ -1,
% _116020#=<_116066+ -1,
% _115996#=<_116020+ -1 ;
% % 189,604 inferences, 0.080 CPU in 0.080 seconds (100% CPU, 2374537 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _97150), tuple([lit(atom, node), lit(atom, leaf), lit(int, _97196), lit(atom, leaf)])]), lit(int, _97220), tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _97288), lit(atom, leaf)]), lit(int, _97312), tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _97380), lit(atom, leaf)]), lit(int, _97404), lit(atom, leaf)])])]),
% _97150#=<_97220+ -1,
% _97150#=<_97196+ -1,
% _97220#=<_97404+ -1,
% _97220#=<_97380+ -1,
% _97220#=<_97312+ -1,
% _97220#=<_97288+ -1,
% _97196#=<_97220+ -1,
% _97380#=<_97404+ -1,
% _97312#=<_97404+ -1,
% _97312#=<_97380+ -1,
% _97288#=<_97312+ -1
%%-- testing generation w/o Coroutining ----------------------------------------
% set_config(max_size(12)).
% gen_avl_tree_typeofFirst((Tree)) :-
%   typeof(Tree,tree(integer)),
%   eval(
%     apply(var('bbst',1),[var('Tree')]),
%     [('Tree',Tree)],
%     lit(atom,true)
%   ).
% ?- time(gen_avl_tree_typeofFirst(Tree)).
% % 338 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 510100 Lips)
% Tree = lit(atom, leaf) ;
% % 6,431 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 1288515 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _39204), lit(atom, leaf)]),
% _39204 in inf..sup ;
% % 12,464 inferences, 0.009 CPU in 0.009 seconds (100% CPU, 1335603 Lips)
% Tree = tuple([lit(atom, node), lit(atom, leaf), lit(int, _87496), tuple([lit(atom, node), lit(atom, leaf), lit(..., ...)|...])]),
% _87496#=<_87542+ -1 ;
% % 3,375,837,108 inferences, 514.566 CPU in 514.571 seconds (100% CPU, 6560553 Lips)
% Tree = tuple([lit(atom, node), tuple([lit(atom, node), lit(atom, leaf), lit(int, _59186), lit(atom, leaf)]), lit(int, _59210), lit(atom, leaf)]),
% _59186#=<_59210+ -1 

-module(avl_insert_bug).
-export([height/1,left_rotate/1,right_rotate/1,avl/1,avl_insert/2]).

-include_lib("proper/include/proper.hrl").

%==============================================================================
%% API for AVL
% AVL tree is a self-balancing binary search tree where the difference between
% heights of left and right subtrees cannot be more than one for all nodes.
% (ref. https://www.geeksforgeeks.org/avl-tree-set-1-insertion/)
% (ref. https://www.tutorialspoint.com/data_structures_algorithms/avl_algorithm.htm)

-type tree(T) :: 'leaf' | {'node', tree(T), T, tree(T)}.

%% height(Tree) returns the height of Tree
-spec height(tree(any())) -> non_neg_integer().
height(T) ->
  case T of
    leaf -> 0;
    {node, Left, _Value, Right} ->
      max(height(Left),height(Right)) + 1
end.

% avl(Tree) returns true if Tree is a balanced binary search tree
-spec avl(tree(any())) -> boolean().
avl(T) ->
  case T of
    % 1. a leaf is a balanced binary search tree
    leaf -> true;
    % 2. a node is a balanced binary search tree if
    {node, Left,Value,Right} ->
      % 2.1 the balance factor of a node (the difference between the
      % heights of the left and right subtrees) belongs to {-1,0,1}
      BalanceF = height(Left) - height(Right),
      BalanceF >= -1 andalso BalanceF =< 1 andalso
      % 2.2 all nodes of the Left subtree are less than Value
      ltt(Left,Value) andalso
      % 2.3 all nodes of the Right subtree are greater than Value
      gtt(Right,Value) andalso
      % 2.4 the Left and the Right subtrees are balanced binary search trees
      avl(Left) andalso avl(Right);
    % returns false if neither 1 nor 2 hold or the input is not a tree
    _ -> false
  end.

%% ltt(Tree,Value) is true if for each node n in Tree, value(n) < Value
-spec ltt(tree(any()),any()) -> boolean().
ltt(T,V) ->
  case T of
    leaf -> true;
    {node, L,X,R} -> X < V andalso ltt(L,V) andalso ltt(R,V);
    _T -> false
  end.

%% all_gt(Tree,Value) is true if for each node n in Tree, value(n) > Value
-spec gtt(tree(any()),any()) -> boolean().
gtt(T,V) ->
  case T of
    leaf -> true;
    {node, L,X,R} -> X > V andalso gtt(L,V) andalso gtt(R,V);
    _T -> false
  end.

%% avl_insert(Elem,Tree) adds Elem to Tree.
-spec avl_insert(T,tree(T)) -> tree(T).
avl_insert(E,T) ->
  case T of
    {node, L,V,R} when E < V ->
      re_balance(E,{node, avl_insert(E,L),V,R});
    {node, L,V,R} when E > V ->
      re_balance(E,{node, L,V,avl_insert(E,R)});
    {node, L,V,R} when E == V ->
      {node, L,V,R};
    leaf ->
      {node, leaf,E,leaf}
  end.

%% re_balance(Elem,Tree) makes Tree balanced
-spec re_balance(T,tree(T)) -> tree(T).
re_balance(E,T) ->
  {node, L,V,R} = T,
  case height(L) - height(R) of
    % Left unbalanced
    2 -> {node, _,LV,_} = L,
      if
        E < LV -> right_rotate(T);
        E > LV -> right_rotate({node, left_rotate(L),V,R})
      end;
    % Right unbalanced
   -2 -> {node, _,RV,_} = R,
      if
        E < RV -> left_rotate(T); % < should be >
        E > RV -> left_rotate({node, L,V,right_rotate(R)}) % > should be <
      end;
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
%% Specification for AVL trees API
% avl: balanced binary serach tree
avl() ->
  ?SUCHTHAT(Tree, tree(integer()), avl(Tree)).
% adding an element Elem to any AVL tree, produces an AVL tree
prop_insert() ->
  ?FORALL({Elem,Tree}, {integer(),avl()}, avl(avl_insert(Elem,Tree))).

tree(G) ->
  union([
    'leaf',
    {'node',G,tree(G),tree(G)}
  ]).

%
% avl_insert_bug:avl(avl_insert_bug:avl_insert(57,{node,{node,leaf,-24,{node,leaf,72,leaf}},82,{node,{node,leaf,91,leaf},94,{node,leaf,95,{node,leaf,100,leaf}}}})).

%%-- testing generation w/o Coroutining ----------------------------------------
% gen_avl_tree_typeofFirst((Tree)) :-
%   typeof(Tree,tree(integer)),
%   eval(
%     apply(var('bbst',1),[var('Tree')]),
%     [('Tree',Tree)],
%     lit(atom,Res)
%   ),
%   ( Res == true -> ( write(Tree), nl ) ; ( write(Res), nl ) ).
% ?- time(gen_avl_tree_typeofFirst(Tree)).
%

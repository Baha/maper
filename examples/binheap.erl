-module(binheap).
%-export([go/0,rbt/1,rbt/0,tree/1,rbt_insert/2, numnodes_tree/2]).


-import('prosyt_lib',[time/1]).
-include_lib("proper/include/proper.hrl").

%==============================================================================
-type bintree(T) :: {'node', T, list(bintree(T))}.

% https://en.wikipedia.org/wiki/Binomial_heap
%
% A binomial tree of order 0 is a single node
% A binomial tree of order k has a root node whose children are roots of binomial trees of orders k−1, k−2, ..., 2, 1, 0 (in this order).
% A binomial tree of order k has 2k nodes, height k.

-spec isbintree(bintree(any()),  integer() ) -> boolean().
isbintree(BT,K) ->
  case BT of
    {node, _, []} when K==0 -> true;
    {node, _, BTs} when K>=1 -> isbintree_list(BTs,K-1);
    _ -> false
  end.

-spec isbintree_list(list(bintree(any())),  integer() ) -> boolean().
isbintree_list(L,K) ->
  case L of
    [] when K==0 -> true;
    [BT|BTs] when K>=1 ->  isbintree(BT,K) andalso isbintree_list(BTs,K-1);
    _ -> false
  end.


-spec minheap(bintree(any())) -> boolean().
minheap(BT) ->
  case BT of
    {node, _ , [] } -> true;
    {node, V,  BTs} -> minheap_list(BTs,V);
    _ -> false
  end.

-spec minheap_list(list(bintree(any())),  integer() ) -> boolean().
minheap_list(L,Min) ->
  case L of
    [] -> true;
    [BT| BTs] ->
      {node, V, _} = BT,
      V > Min andalso  minheap(BT) andalso minheap_list(BTs,Min);
    _ -> false
  end.

isbintree_minheap(BT,K) ->
  isbintree(BT,K) andalso minheap(BT).

%%% add Binomial_heap

bintree() ->
  ?SUCHTHAT({Tree,K}, { bintree(integer()),integer() }, isbintree(Tree,K)).
% adding an element Elem to any AVL tree, produces an AVL tree
prop_tbt() ->
  ?FORALL({Tree,K}, bintree(), true).

bintree(T) ->
  union([
    {'node', T, list(bintree(T))}
  ]).

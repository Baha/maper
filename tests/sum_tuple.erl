-module(sum_tuple).
-export([main/2]).

main(X, Y) ->
  Tuple = {X,Y},
  sum_tuple(Tuple).

sum_tuple({X,Y}) -> X + Y.

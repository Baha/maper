-module(cases_primop_tuple).
-export([main/1]).

main(X) ->
  case X of
    1 -> one;
    {2,3} -> two
  end.


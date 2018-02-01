-module(cases_primop).
-export([main/1]).

main(X) ->
  case X of
    1 -> one;
    2 -> two
  end.

-module(cases).
-export([main/1]).

main(X) ->
  case X of
    1 -> one;
    2 -> two;
    _ -> else
  end.

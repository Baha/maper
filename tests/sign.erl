-module(sign).
-export([main/1]).

main(X) ->
  case X of
    Y when Y > 0 -> pos;
    Y when Y < 0 -> neg;
    _ -> zero
  end.

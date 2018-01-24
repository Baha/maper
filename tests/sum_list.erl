-module(sum_list).
-export([main/1]).

main(L) ->
  case L of
    [] -> 0;
    [H|T] -> H + main(T)
  end.

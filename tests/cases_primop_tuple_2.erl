-module(cases_primop).
-export([main/1]).

main(X) ->
  case { X , X + 2 } of
    { 1 , 3 } -> ok
  end.


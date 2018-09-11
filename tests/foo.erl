-module(foo).

-export([foo/2]).

foo(X,{Y,Z}) ->
  Z = case h(X) of
    1 -> g(h(Y));
    2 -> h(g(Y))
  end,
  Z+1.

g(X) -> X + 1.

h(X) -> X - 1.

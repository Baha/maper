-module(enc_apply).
-export([main/1]).
-import(odd,[odd/1]).

main(X) ->
  odd(X),
  hello:main(),
  % fact(X), % enc_apply.erl:8: function fact/1 undefined
  apply(factorial,fact,[X]),
  apply(fact,[X]).

-module(enc_seq).
-export([main/1]).

main(X) ->
  weird_seq(X).%,
  %a_seq(X).

weird_seq(X) ->
  _ = X + 7,
  _ = 21 + X,
  _ = [X].

a_seq(X) ->
  Y = X + 5,
  Z = 42 + Y,
  _ = [X|[Z]].

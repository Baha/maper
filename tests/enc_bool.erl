-module(enc_bool).
-export([main/0]).

a(X,Y) -> X and Y.
o(X,Y) -> X or Y.
n(X) -> not X.
x(X,Y) -> X xor Y.
oe(X,Y) -> X orelse Y.
aa(X,Y) -> X andalso Y.


main() ->
  a(true,true),
  o(false,true),
  n(false),
  x(true,false),
  oe(false,true),
  aa(true,true).

-module(enc_tuple).
-export([main/1]).

main(X) ->
  case X of
    {} -> [];
    {a,b,c} -> [a,b,c];
    {S,z,f} -> [z,a]
  end.

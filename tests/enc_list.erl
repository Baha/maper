-module(enc_list).
-export([main/1]).

main(X) ->
  case X of
   [a,b,c] -> [d,e];
   [S|[Z|[]]] -> [S,Z];
   [f,g|h] -> 'notAproperList';
   [H|T] when is_list(T) -> [H|T]
  end.

-module(biggest_ok).

-include_lib("proper/include/proper.hrl").

biggest([Head | Tail]) ->
    biggest(Tail, Head).

biggest([], Biggest) ->
    Biggest;
biggest([Head|Tail], Biggest) when Head >= Biggest -> 
    biggest(Tail, Head);
biggest([Head|Tail], Biggest) when Head < Biggest ->
    biggest(Tail, Biggest).

prop_biggest() ->
    ?FORALL(List, non_empty(list(integer(2,5))),
        begin
            biggest(List) =:= lists:last(lists:sort(List))
        end).

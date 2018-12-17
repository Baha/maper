-module(sorted_ok).

-include_lib("proper/include/proper.hrl").

is_ordered([A,B|T]) ->
    A =< B andalso is_ordered([B|T]);
is_ordered(_) -> % smaller lists
    true.

prop_sort() ->
    ?FORALL(List, list(term()),
        begin
            is_ordered(lists:sort(List))
        end).

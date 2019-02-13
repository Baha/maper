-module(testing).
-export([parse_input/2]).

parse_input(Input, PropFun) ->
  % check if PropFun expects composite
  PropFormat = lists:nth(2, PropFun),
  PInput =
    case is_composite(PropFormat) of
      true ->
        convert_input(Input);
      false ->
        ok
    end,
  PInput.

is_composite(Format) when is_tuple(Format) -> true;
is_composite(Format) when is_list(Format) -> true;
is_composite(_) -> false.

convert_input(X) -> X.

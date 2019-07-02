-module(stack_bug).
-export([is_empty/1, size/1, new/0, push/2, pop/1]).
-export_type([stack/1]).

%% NOTE: Compile this file with debug_info enabled.

-include_lib("proper/include/proper.hrl").

-opaque stack(T) :: {non_neg_integer(),[T]}.

%% NOTE: Every instance of the ADT in a spec must have variables as parameters.
%%	 When this would mean singleton variables, use variables starting with
%%	 an underscore.
-spec is_empty(stack(_T)) -> boolean().
is_empty({0, []}) ->
    true;
is_empty({_N, [_Top|_Rest]}) ->
    false.

-spec size(stack(_T)) -> non_neg_integer().
size({N, _Elems}) ->
    N.

-spec new() -> stack(_T).
new() ->
    {0, []}.

-spec push(T, stack(T)) -> stack(T).
push(X, {N,Elems}) ->
    {N+1, [X|Elems]}.

-spec pop(stack(T)) -> {T,stack(T)}.
pop({0, []}) ->
    throw(stack_empty);
pop({N, [Top|Rest]}) when N > 1 ->  % N > 1 should be N > 0
    {Top, {N-1,Rest}}.

%==============================================================================
%% Properties of stacks

is_stack({N,S}) -> N =:= length(S).

wfs_stack() ->
  ?SUCHTHAT(S, stack(integer()), is_stack(S)).

prop_tbt() ->
  ?FORALL({X,S}, {integer(), wfs_stack()},
    begin {Y,_} = pop(push(X,S)), X =:= Y end ).

% the following declaration is added to avoid the erlang compiler error:
%   ./examples/stack_bug.erl:44: function stack/1 undefined
% due to the use tree(integer()) in the ?SUCHTHAT macro
% It is not used in the generation process.
stack(T) ->
  union([
    non_neg_integer(),
    list(T)
  ]).

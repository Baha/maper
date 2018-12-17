-module(stack).
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
pop({N, [Top|Rest]}) when N > 0 ->
    {Top, {N-1,Rest}}.

%==============================================================================

%% Properties of stacks

prop_push_pop() ->
    ?FORALL({X,S}, {integer(),stack(integer())},
	    begin
		{Y,_} = pop(push(X,S)),
		X =:= Y
	    end).

%==============================================================================

%% Stack generator

%% Comment out the generator and recompile to use the type instead.

stack(T) ->
    well_defined(?SIZED(S, stack(S, T))).

stack(0, _) ->
    {'$call',stack,new,[]};
stack(S, G) ->
    frequency([{1, ?LAZY(stack(0,G))},
	       {5, ?LAZY(?LETSHRINK([St],
				    [stack(S - 1, G)],
				    {'$call',stack,push,[G,St]}))},
	       {5, ?LAZY(?LETSHRINK([St],
				    [stack(S - 1, G)],
				    {'$call',erlang,element,
				     [2,{'$call',stack,pop,[St]}]}))}
	      ]).

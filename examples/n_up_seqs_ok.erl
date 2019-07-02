-module(n_up_seqs_ok).
-export([nupseqs/2,appendall/1,all_up/1,nupseqs_max/2]).

-include_lib("proper/include/proper.hrl").

%==============================================================================
nupseqs_max(1,[L]) ->
  lists:max(L);
nupseqs_max(N,[L|Ls]) ->
  max(lists:max(L),nupseqs_max(N-1,Ls)).

up_seq(L) ->
  case L of
    [] -> true;
    [_] -> true;
    [A,B] when A =< B -> true;
    [A,B|T] when A =< B -> up_seq([B|T]);
    _ -> false
  end.

all_up([X]) ->
  up_seq(X);
all_up([X,Y|Zs]) ->
  up_seq(X),
  length(Y) - length(X) == 1 andalso all_up([Y|Zs]);
all_up(_) -> false.

appendall([Ys]) -> Ys;
appendall([[]|Ys]) ->
  appendall(Ys);
appendall([[X|Xs]|Ys]) ->
  [X|appendall([Xs|Ys])].

nupseqs(N,L) -> appendall(L), N == length(L) andalso all_up(L).

%==============================================================================
%% Filter and Property

uplists() ->
  ?SUCHTHAT({N,L}, {integer(2,10),list(non_empty(list(integer)))}, nupseqs(N,L)).

prop_tbt() ->
  ?FORALL({N,L}, uplists(), nupseqs_max(N,L) =:= lists:max(lists:flatten(L))).

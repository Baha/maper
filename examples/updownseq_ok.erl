-module(updownseq_ok).
-export([updownseq/3]).

-include_lib("proper/include/proper.hrl").

%===============================================================================

updown() ->
  ?SUCHTHAT({L,L1,L2}, { list(integer()), list(integer()), list(integer()) },
    updownseq(L,L1,L2)).


up_seq(L) ->
  case L of
    [] -> true;
    [_] -> true;
    [A,B] when A =< B -> true;
    [A,B|T] when A =< B -> up_seq([B|T]);
    _ -> false
  end.

down_seq(L) ->
  case L of
    [] -> true;
    [_] -> true;
    [A,B] when A >= B -> true;
    [A,B|T] when A >= B -> down_seq([B|T]);
    _ -> false
  end.

append([],Ys) -> Ys;
append([X|Xs],Ys) ->
  [X|append(Xs,Ys)].

updownseq(L,L1,L2) ->
  case append(L1,L2) of
    L -> Len = length(L1) - length(L2),
         Len >= -1 andalso Len =< 1 andalso up_seq(L1) andalso down_seq(L2);
    _ -> false
  end.

%==============================================================================
%% Property for updownseq_ok

prop_tbt() ->
  ?FORALL({L,U,D}, updown(),
    begin
      lists:last(U) =:= lists:max(L) andalso
      lists:last(D) =:= lists:min(L)
    end).

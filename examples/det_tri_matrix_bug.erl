-module(det_tri_matrix_ok).
-export([det_tri_up_matrix/1,det_tri_lo_matrix/1]).

-include_lib("proper/include/proper.hrl").

%==============================================================================
%% Determinant of a triangular matrix

det_tri_up_matrix([]) -> 1;
det_tri_up_matrix([[R|_]|Rows]) ->
  R * det_tri_up_matrix(Rows).

det_tri_lo_matrix([]) -> 1;
det_tri_lo_matrix([Row|Rows]) ->
  lists:last(Row) * det_tri_lo_matrix(Rows).

transpose(T) ->
  lists:reverse(T). % bug

%==============================================================================
%% Filter

tri_up_matrix() ->
  ?SUCHTHAT(M, list(non_empty(list(float))), tri_up_matrix(M)).

%% Filter's utility predicates

tri_up_matrix([R]) ->
  length(R) == 1;
tri_up_matrix([R1,R2|Rs]) ->
  L1 = length(R1),
  L2 = length(R2),
  L1 - L2 == 1 andalso tri_up_matrix([R2|Rs]);
tri_up_matrix(_) -> false.

%==============================================================================
%% Property

prop_tbt() ->
  ?FORALL(M, tri_up_matrix(),
    det_tri_up_matrix(M) =:= det_tri_lo_matrix(transpose(M))).

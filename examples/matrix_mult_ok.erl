-module(matrix_mult_ok).
-export([multiply/2,compatible_matrices/2,matrix_dimensions/1,matrix/1]).

-include_lib("proper/include/proper.hrl").

%==============================================================================
%% Multiplies two matrices. Usage example:
%% $ matrix_mult_bug:multiply([[1,2,3],[4,5,6]], [[4,4],[0,0],[1,4]])
%% If the dimentions are incompatible, an error is thrown.

% The original code is borrowed from:
%     https://rosettacode.org/wiki/Matrix_multiplication#Erlang

transpose([[]|_]) -> [];
transpose(B) -> [lists:map(fun hd/1, B) | transpose(lists:map(fun tl/1, B))].


red(Pair, Sum) ->
  X = element(1, Pair),   %gets X
  Y = element(2, Pair),   %gets Y
  X * Y + Sum.

%% Mathematical dot product. A x B = d
%% A, B = 1-dimension vector
%% d    = scalar
dot_product(A, B) ->
  lists:foldl(fun red/2, 0, lists:zip(A, B)).


%% Exposed function. Expected result is C = A x B.
multiply(A, B) ->
  %% First transposes B, to facilitate the calculations (It's easier to fetch
  %% row than column wise).
  multiply_internal(A, transpose(B)).


%% This function does the actual multiplication, but expects the second matrix
%% to be transposed.
multiply_internal([Head | Rest], B) ->
  % multiply each row by Y
  Element = multiply_row_by_col(Head, B),
  % concatenate the result of this multiplication with the next ones
  [Element | multiply_internal(Rest, B)];
% concatenating and empty list to the end of a list, changes nothing.
multiply_internal([], _B) -> [].


multiply_row_by_col(Row, [Col_Head | Col_Rest]) ->
  Scalar = dot_product(Row, Col_Head),
  [Scalar | multiply_row_by_col(Row, Col_Rest)];
multiply_row_by_col(_Row, []) -> [].

%==============================================================================
%% Filter

two_compatible_matrices() ->
    ?SUCHTHAT({M1,M2}, { list(list(float)), list(list(float)) },
      compatible_matrices(M1,M2)).

%% Filter's utility predicates

wfm_matrix(C,[R]) ->
  C == length(R);
wfm_matrix(C,[R|Rs]) ->
  C == length(R) andalso wfm_matrix(C,Rs);
wfm_matrix(_,_) -> false.

matrix([Row|Rows]) ->
  wfm_matrix(length(Row),Rows).

matrix_dimensions([Row|Rows]) ->
  { length([Row|Rows]), length(Row) }. % { #rows , #columns }

compatible_matrices(M1,M2) ->
  case matrix(M1) andalso matrix(M2) of
    true -> {_,C} = matrix_dimensions(M1),
            {R,_} = matrix_dimensions(M2),
            C == R;
    _ -> false
  end.

%==============================================================================
%% Property

prop_tbt() ->
    ?FORALL({M1,M2}, two_compatible_matrices(),
        begin
            {M,N} = matrix_dimensions(M1),
            {N,P} = matrix_dimensions(M2),
            {M,P} =:= matrix_dimensions(multiply(M1,M2))
        end).

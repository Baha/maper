% http://erlang.org/doc/man/erlang.html
:- module(erlang, []).

:- use_module(library(clpfd)).
:- use_module(library(clpr)).

bif_pred(Pred) :-
  memberchk(Pred,[
    '=='/2,                  % [term(),term()] -> boolean()
    '/='/2,                  % [term(),term()] -> boolean()
    '<'/2,                   % [term(),term()] -> boolean()
    '=<'/2,                  % [term(),term()] -> boolean()
    '>'/2,                   % [term(),term()] -> boolean()
    '>='/2,                  % [term(),term()] -> boolean()
    '=:='/2,                 % [term(),term()] -> boolean()
    '=/='/2,                 % [term(),term()] -> boolean()
    '+'/2,                   % [number(),number()] -> integer()
    '-'/2,                   % [number(),number()] -> integer()
    '*'/2,                   % [number(),number()] -> integer()
    '/'/2,                   % [number(),number()] -> integer()
    'div'/2,                 % [integer(),integer()] -> integer()
    'rem'/2,                 % [integer(),integer()] -> integer()
    '+'/1,                   % [number()] -> number()
    '-'/1,                   % [number()] -> number()
    'and'/2,                 % [boolean(),boolean()] -> boolean()
    'or'/2,                  % [boolean(),boolean()] -> boolean()
    'xor'/2,                 % [boolean(),boolean()] -> boolean()
    'not'/1,                 % [boolean(),boolean()] -> boolean()
    '++'/2,                  % [list(),list()] -> list()
    '--'/2,                  % [list(),list()] -> list()
    'abs'/1,                 % [number()] -> number()
    'append_element'/2,      % [tuple(),term()] -> tuple()
    'apply'/2,               % [tuple(),[term()]] -> tuple()
    'apply'/3,               % [module(),atom(),[term()]] -> tuple()
    'atom_to_list'/1,        % [atom()] -> list()
    'ceil'/1,                % [number()] -> integer()
    'delete_element'/2,      % [integer(),tuple()] -> tuple()
    'element'/2,             % [integer(),tuple()] -> term()
    'error'/1,               % [term()] -> no_return()
    'error'/2,               % [term(),[term()]] -> no_return()
    'float'/1,               % [number()] -> float()
    'floor'/1,               % [number()] -> integer()
    'hd'/1,                  % [list()] -> term()
    'insert_element'/3,      % [integer(),tuple(),term()] -> tuple()
    'integer_to_list'/1,     % [integer()] -> string()
    'is_atom'/1,             % [term()] -> boolean()
    'is_float'/1,            % [term()] -> boolean()
    'is_integer'/1,          % [term()] -> boolean()
    'is_list'/1,             % [term()] -> boolean()
    'is_number'/1,           % [term()] -> boolean()
    'is_tuple'/1,            % [term()] -> boolean()
    'length'/1,              % [list()] -> integer()
    'max'/2,                 % [term(),term()] -> term()
    'min'/2,                 % [term(),term()] -> term()
    'nif_error'/1,           % [term()] -> no_return()
    'nif_error'/2,           % [term(),[term()]] -> no_return()
    'round'/1,               % [number()] -> integer()
    'size'/1,                % [tuple() | binary()] -> integer()
    'tl'/1,                  % [list()] -> term()
    'trunc'/1,               % [number()] -> integer()
    'tuple_size'/1           % [tuple()] -> integer()
  ]).

:- discontiguous bif/3.

% 8.11 Term Comparisons --------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#term-comparisons
% number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
/** ----------------------------------------------------------------------------
* ==
*/
bif('==',[L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), eq(L1,L2)).
bif('==',[L1,L2], lit(atom,false)) :-
  bif('/=',[L1,L2], lit(atom,true)).
% number
eq(lit(int,X),lit(int,Y)) :- X #= Y.
eq(lit(int,X),lit(float,Y)) :- { X =:= Y }.
eq(lit(float,X),lit(int,Y)) :- { X =:= Y }.
eq(lit(float,X),lit(float,Y)) :- { X =:= Y }.
% atom
eq(lit(atom,X),lit(atom,Y)) :- compare(=,X,Y).
% tuple
eq(tuple(X),tuple(Y)) :-
  bif('tuple_size',[tuple(X)], lit(int,S1)),
  bif('tuple_size',[tuple(Y)], lit(int,S2)),
  S1 #= S2,
  eq_tuple(X,Y).
% list
eq(nil,nil).
eq(cons(X,Xs),cons(Y,Ys)) :-
  bif('==',[X,Y], lit(atom,true)),
  eq(Xs,Ys).
%%
eq_tuple([],[]).
eq_tuple([X|Xs],[Y|Ys]) :-
  bif('==',[X,Y], lit(atom,true)),
  when((nonvar(Xs),nonvar(Ys)), eq_tuple(Xs,Ys)).
/** ----------------------------------------------------------------------------
 * /=
 */
bif('/=', [L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), neq(L1,L2) ).
bif('/=', [L1,L2], lit(atom,false)) :-
  bif('==', [L1,L2], lit(atom,true)).
% number
neq(lit(int,X),lit(int,Y)) :- X #\= Y.
neq(lit(int,X),lit(float,Y)) :- { X =\= Y }.
neq(lit(float,X),lit(int,Y)) :- { X =\= Y }.
neq(lit(float,X),lit(float,Y)) :- { X =\= Y }.
% atom
neq(lit(atom,X),lit(atom,Y)) :- dif(X,Y).
% tuple
neq(tuple(X),tuple(Y)) :-
  bif('tuple_size',[tuple(X)], lit(int,S1)),
  bif('tuple_size',[tuple(Y)], lit(int,S2)),
  neq_tuple(X,S1,Y,S2).
% list
neq(cons(X,Xs),cons(Y,Ys)) :-
  bif('/=',[X,Y], lit(atom,Res)),
  neq_cons_cont(Res,Xs,Ys).
% different types
neq(X,Y) :- type_of(X,T1), type_of(Y,T2), dif(T1,T2).
%%
neq_tuple(_X,S1,_Y,S2) :-
  S1 #\= S2.
neq_tuple(X,S1,Y,S2) :-
  S1 #= S2,
  neq_tuple(X,Y).
%
neq_tuple([X|Xs],[Y|Ys]) :-
  bif('/=',[X,Y], lit(atom,Res)),
  neq_tuple_cont(Res,Xs,Ys).
%
neq_tuple_cont(true,_Xs,_Ys).
neq_tuple_cont(false,Xs,Ys) :-
  when((nonvar(Xs),nonvar(Ys)), neq_tuple(Xs,Ys)).
%%
neq_cons_cont(true,_Xs,_Ys).
neq_cons_cont(false,Xs,Ys) :-
  bif('/=',[Xs,Ys], lit(atom,true)).
/** ----------------------------------------------------------------------------
 * =<
 */
bif('=<',[L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), lte(L1,L2)).
bif('=<',[L1,L2], lit(atom,false)) :-
  bif('>',[L1,L2], lit(atom,true)).
%% utility predicates
% number
lte(lit(int,X),lit(int,Y)) :- X #=< Y.
lte(lit(int,X),lit(float,Y)) :- { X =< Y }.
lte(lit(float,X),lit(int,Y)) :- { X =< Y }.
lte(lit(float,X),lit(float,Y)) :- { X =< Y }.
% atom
lte(lit(atom,X),lit(atom,Y)) :- X @=< Y.
% tuple : tuples are ordered by size, two tuples with the same size are
% compared element by element.
lte(tuple(X),tuple(Y)) :-
  bif('tuple_size',[tuple(X)], lit(int,S1)),
  bif('tuple_size',[tuple(Y)], lit(int,S2)),
  lte_tuple_cont(X,S1,Y,S2).
% list: lists are compared element by element
lte(nil,nil).
lte(cons(X,Xs),cons(Y,Ys)) :-
  bif('=<',[X,Y], lit(atom,true)),
  bif('=<',[Xs,Ys], lit(atom,true)).
% different types (for X and Y of the same type we have the rules above)
lte(X,Y) :- type_of(X,T1), type_of(Y,T2), lt_by_type(T1,T2).
%%
lte_tuple_cont(_T1,S1,_T2,S2) :-
  S1 #\= S2.
lte_tuple_cont(T1,S1,T2,S2) :-
  S1 #= S2,
  when((nonvar(T1),nonvar(T2)), lte_tuple(T1,T2) ).
%%
lte_tuple([],[]).
lte_tuple([X|_Xs],[Y|_Ys]) :-
  bif('=<',[X,Y], lit(atom,true)),
  when( (nonvar(Xs),nonvar(Ys)), lte_tuple(Xs,Ys) ).
%%
type_of(lit(int,_),number). type_of(lit(float,_),number).
type_of(lit(atom,_),atom).
type_of(tuple(_),tuple).
type_of(nil,nil).
type_of(cons(_,_),list).
/** ----------------------------------------------------------------------------
 *  <
 */
bif('<',[L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), lt(L1,L2)).
bif('<',[L1,L2], lit(atom,false)) :-
  bif('>=',[L1,L2], lit(atom,true)).
%% utility predicates
% number
lt(lit(int,X),lit(int,Y)) :- X #< Y.
lt(lit(int,X),lit(float,Y)) :- { X < Y }.
lt(lit(float,X),lit(int,Y)) :- { X < Y }.
lt(lit(float,X),lit(float,Y)) :- { X < Y }.
% atom
lt(lit(atom,X),lit(atom,Y)) :- X @< Y.
% tuple : tuples are ordered by size, two tuples with the same size are
% compared element by element.
lt(tuple(X),tuple(Y)) :-
  bif('tuple_size',[tuple(X)], lit(int,S1)),
  bif('tuple_size',[tuple(Y)], lit(int,S2)),
  lt_tuple_cont(X,S1,Y,S2).
% list: lists are compared element by element
lt(cons(X,_Xs),cons(Y,_Ys)) :-
  bif('<',[X,Y], lit(atom,true)).
lt(cons(X,Xs),cons(Y,Ys)) :-
  bif('==',[X,Y], lit(atom,true)),
  bif('<',[Xs,Ys], lit(atom,true)).
% different types
lt(X,Y) :- type_of(X,T1), type_of(Y,T2), lt_by_type(T1,T2).
%%
lt_tuple_cont(_T1,S1,_T2,S2) :-
  S1 #\= S2.
lt_tuple_cont(T1,S1,T2,S2) :-
  S1 #= S2,
  when((nonvar(T1),nonvar(T2)), lt_tuple(T1,T2) ).
%%
lt_tuple([X|_Xs],[Y|_Ys]) :-
  bif('<',[X,Y], lit(atom,true)).
lt_tuple([X|Xs],[Y|Ys]) :-
  bif('==',[X,Y], lit(atom,true)),
  when((nonvar(Xs),nonvar(Ys)), lt_tuple(Xs,Ys)).
%%
:- use_module(library(tabling)). % just a note (unnecessary thanks to when/2)
:- table lt_by_type/2.
% http://www.swi-prolog.org/pldoc/man?section=tabling-non-termination
lt_by_type(number,atom).
lt_by_type(atom,tuple).
lt_by_type(tuple,nil).
lt_by_type(nil,list).
lt_by_type(X,Z) :- lt_by_type(X,Y), lt_by_type(Y,Z).
/** ----------------------------------------------------------------------------
 * >=
 */
bif('>=', [L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), gte(L1,L2)).
bif('>=', [L1,L2], lit(atom,false)) :-
  bif('<', [L1,L2], lit(atom,true)).
%% utility predicates
% number
gte(lit(int,X),lit(int,Y)) :- X #>= Y.
gte(lit(int,X),lit(float,Y)) :- { X >= Y }.
gte(lit(float,X),lit(int,Y)) :- { X >= Y }.
gte(lit(float,X),lit(float,Y)) :- { X >= Y }.
% atom
gte(lit(atom,X),lit(atom,Y)) :- X @>= Y.
% tuple: tuples are ordered by size, two tuples with the same size are
% compared element by element.
gte(tuple(X),tuple(Y)) :-
  bif('tuple_size',[tuple(X)], lit(int,S1)),
  bif('tuple_size',[tuple(Y)], lit(int,S2)),
  gte_tuple_cont(X,S1,Y,S2).
% list: lists are compared element by element
gte(nil,nil).
gte(cons(X,Xs),cons(Y,Ys)) :-
  bif('>=',[X,Y], lit(atom,true)),
  bif('>=',[Xs,Ys], lit(atom,true)).
% different types
gte(X,Y) :- type_of(X,T1), type_of(Y,T2), lt_by_type(T2,T1).
% gt_tuple
gte_tuple_cont(_T1,S1,_T2,S2) :-
  S1 #> S2.
gte_tuple_cont(T1,S1,T2,S2) :-
  S1 #= S2,
  when((nonvar(T1),nonvar(T2)), gte_tuple(T1,T2)).
%%
gte_tuple([],[]).
gte_tuple([X|Xs],[Y|Ys]) :-
  bif('>=',[X,Y], lit(atom,true)),
  when((nonvar(Xs),nonvar(Ys)), gte_tuple(Xs,Ys)).
/** ----------------------------------------------------------------------------
* >
*/
bif('>',[L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), gt(L1,L2)).
bif('>',[L1,L2], lit(atom,false)) :-
  bif('=<',[L1,L2], lit(atom,true)).
%% utility predicates
% number
gt(lit(int,X),lit(int,Y)) :- X #> Y.
gt(lit(int,X),lit(float,Y)) :- { X > Y }.
gt(lit(float,X),lit(int,Y)) :- { X > Y }.
gt(lit(float,X),lit(float,Y)) :- { X > Y }.
% atom
gt(lit(atom,X),lit(atom,Y)) :- X @> Y.
% tuple: tuples are ordered by size, two tuples with the same size are
% compared element by element.
gt(tuple(X),tuple(Y)) :-
  bif('tuple_size',[tuple(X)], lit(int,S1)),
  bif('tuple_size',[tuple(Y)], lit(int,S2)),
  gt_tuple_cont(X,S1,Y,S2).
% list: lists are compared element by element
gt(cons(X,_Xs),cons(Y,_Ys)) :-
  bif('>',[X,Y], lit(atom,true)).
gt(cons(X,Xs),cons(Y,Ys)) :-
  bif('==',[X,Y], lit(atom,true)),
  bif('>',[Xs,Ys], lit(atom,true)).
% different types
gt(X,Y) :- type_of(X,T1), type_of(Y,T2), lt_by_type(T2,T1).
% gt_tuple
gt_tuple_cont(_T1,S1,_T2,S2) :-
  S1 #> S2.
gt_tuple_cont(T1,S1,T2,S2) :-
  S1 #= S2,
  when((nonvar(T1),nonvar(T2)), gt_tuple(T1,T2) ).
%%
gt_tuple([X|_Xs],[Y|_Ys]) :-
  bif('>',[X,Y], lit(atom,true)).
gt_tuple([X|Xs],[Y|Ys]) :-
  bif('==',[X,Y], lit(atom,true)),
  when((nonvar(Xs),nonvar(Ys)), gt_tuple(Xs,Ys)).
/** ----------------------------------------------------------------------------
* =:=
*/
bif('=:=',[L1,L2], lit(atom,true)) :-
  when((nonvar(L1),nonvar(L2)), L1 = L2).
%bif('=:=',[L1,L2], lit(atom,true)) :-
%  L1 = L2.
bif('=:=',[L1,L2], lit(atom,false)) :-
  bif('=/=',[L1,L2], lit(atom,true)).
/** ----------------------------------------------------------------------------
 * =/=
 */
bif('=/=',[L1,L2], lit(atom,true)) :-
  dif(L1,L2).
bif('=/=',[L1,L2], lit(atom,false)) :-
  bif('=:=',[L1,L2], lit(atom,true)).

% 8.12 Arithmetic Expressions --------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#arithmetic-expressions
/** ----------------------------------------------------------------------------
 * binary arithmetic operations (+,-,*,/)
 */
bif(Op,[L1,L2], L3) :-
  memberchk(Op,['+','-','*','/']),
  binary_arith_bif_res(L1,Op,L2, L3).
%
binary_arith_bif_res(lit(int,X),Op,lit(int,Y), lit(int,Z)) :-
  OpCall =.. [Op,X,Y], Z #= OpCall.
binary_arith_bif_res(lit(int,X),Op,lit(float,Y), lit(float,Z)) :-
  OpCall =.. [Op,X,Y], { Z = OpCall }.
binary_arith_bif_res(lit(float,X),Op,lit(_T2,Y), lit(float,Z)) :-
  OpCall =.. [Op,X,Y], { Z = OpCall }.
%
bif(Op,[Arg1,Arg2], error(badarith)) :-
  memberchk(Op,['+','-','*','/']),
  in1_in2_abif_wrong_types(Arg1,Arg2).
%
in1_in2_abif_wrong_types(Arg1,Arg2) :-
  var(Arg1),
  when(nonvar(Arg2), ( Arg2 \= lit(int,_), Arg2 \= lit(float,_) )).
in1_in2_abif_wrong_types(Arg1,Arg2) :-
  nonvar(Arg1),
  ( Arg1 = lit(int,_) ; Arg1 = lit(float,_) ),
  when(nonvar(Arg2), ( Arg2 \= lit(int,_), Arg2 \= lit(float,_) )).
in1_in2_abif_wrong_types(Arg1,Arg2) :-
  var(Arg2),
  when(nonvar(Arg1), ( Arg1 \= lit(int,_), Arg1 \= lit(float,_) )).
in1_in2_abif_wrong_types(Arg1,Arg2) :-
  nonvar(Arg2),
  ( Arg2 = lit(int,_) ; Arg2 = lit(float,_) ),
  when(nonvar(Arg1), ( Arg1\= lit(int,_), Arg1 \= lit(float,_) )).
in1_in2_abif_wrong_types(Arg1,Arg2) :-
  nonvar(Arg1), ( Arg1 \= lit(int,_), Arg1 \= lit(float,_) ),
  nonvar(Arg2), ( Arg2 \= lit(int,_), Arg2 \= lit(float,_) ).

/** ----------------------------------------------------------------------------
 * div
 */
bif(div,[lit(int,X),lit(int,Y)], lit(int,Q)) :-
  when((nonvar(X),nonvar(Y)), idiv(X,Y, Q,_R) ).
%
idiv(N,D, Q,R) :- N #>= 0, D #>= 1, N #= Q*D+R, 0 #=< R, R #< D.
idiv(N,D, Q,R) :- N #>= 0, D #=< -1, N #= Q*D+R, 0 #=< R, R #< -D.
% if D<0, then the remainder is |D| < R =< 0
idiv(N,D, Q,R) :- N #=< -1, D #>= 1, N #= Q*D+R, -D #< R, R #=< 0.
idiv(N,D, Q,R) :- N #=< -1, D #=< -1, N #= Q*D+R, D #< R, R #=< 0.

bif(div,[Arg1,Arg2], error(badarith)) :-
  var(Arg1),
  when(nonvar(Arg2), ( Arg2 \= lit(int,_) ; (Arg2 = lit(int,Y), Y #= 0) )).
bif(div,[Arg1,Arg2], error(badarith)) :-
  nonvar(Arg1), Arg1 = lit(int,_),
  when(nonvar(Arg2), ( Arg2 \= lit(int,_) ; (Arg2 = lit(int,Y), Y #= 0) )).
bif(div,[Arg1,Arg2], error(badarith)) :-
  var(Arg2),
  when(nonvar(Arg1), Arg1 \= lit(int,_)).
bif(div,[Arg1,Arg2], error(badarith)) :-
  nonvar(Arg2), Arg2 = lit(int,_),
  when(nonvar(Arg1), Arg1 \= lit(int,_)).
bif(div,[Arg1,Arg2], error(badarith)) :-
  nonvar(Arg1), Arg1 \= lit(int,_),
  nonvar(Arg2), Arg2 \= lit(int,_).

/** ----------------------------------------------------------------------------
 * rem
 */
bif(rem,[lit(int,X),lit(int,Y)], lit(int,R)) :-
  when((nonvar(X),nonvar(Y)), idiv(X,Y, _Q,R) ).
%
bif(rem,[Arg1,Arg2], error(badarith)) :-
  bif(div,[Arg1,Arg2], error(badarith)).

/** ----------------------------------------------------------------------------
 * unary +
 */
bif('+',[lit(int,X)], lit(int,X)).
bif('+',[lit(float,X)], lit(float,X)).
%
bif('+',[Arg], error(badarith)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) ) ).

/** ----------------------------------------------------------------------------
 * unary -
 */
bif('-',[lit(int,X)], lit(int,Z)) :- Z #= -X.
bif('-',[lit(float,X)], lit(float,Z)) :- { Z = -X }.
%
bif('-',[lit(int,X)], lit(int,X)).
bif('-',[lit(float,X)], lit(float,X)).
%
bif('-',[Arg], error(badarith)) :-
  bif('+',[Arg], error(badarith)).

% 8.13 Boolean Expressions -----------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#boolean-expressions
/** ----------------------------------------------------------------------------
 * not
 */
bif('not',[lit(atom,B)], lit(atom,NotB)) :-
  when(nonvar(B), not(B,NotB) ).
%
not(true , false).
not(false, true ).
%
bif('not',[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= lit(atom,_) ).
/** ----------------------------------------------------------------------------
 * and
 */
bif('and',[lit(atom,B1),lit(atom,B2)], B1andB2) :-
  when((nonvar(B1),nonvar(B2)), and(B1,B2,B1andB2) ).
%
and(true ,true , true ).
and(true ,false, false).
and(false,true , false).
and(false,false, false).
%
bif('and',[Arg1,Arg2], error(badarg)) :-
  var(Arg1),
  when(nonvar(Arg2), Arg2 \= lit(atom,_) ).
bif('and',[Arg1,Arg2], error(badarg)) :-
  var(Arg1), Arg1 = lit(atom,_),
  when(nonvar(Arg2), Arg2 \= lit(atom,_) ).
bif('and',[Arg1,Arg2], error(badarg)) :-
  var(Arg2),
  when(nonvar(Arg1), Arg1 \= lit(atom,_) ).
bif('and',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg2), Arg2 = lit(atom,_),
  when(nonvar(Arg1), Arg1 \= lit(atom,_) ).
bif('and',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg1), Arg1 \= lit(atom,_),
  nonvar(Arg2), Arg2 \= lit(atom,_).
/** ----------------------------------------------------------------------------
 * or
 */
bif('or',[lit(atom,B1), lit(atom,B2)], B1orB2) :-
  when((nonvar(B1),nonvar(B2)), or(B1,B2,B1orB2) ).
%
or(true ,true , true ).
or(true ,false, true ).
or(false,true , true ).
or(false,false, false).
%
bif('or',[Arg1,Arg2], error(badarg)) :-
  bif('and',[Arg1,Arg2], error(badarg)).
/** ----------------------------------------------------------------------------
 * xor
 */
bif('xor',[lit(atom,B1), lit(atom,B2)], B1xorB2) :-
  when((nonvar(B1),nonvar(B2)), xor(B1,B2,B1xorB2) ).
%
xor(true ,true , false).
xor(true ,false, true ).
xor(false,true , true ).
xor(false,false, false).
%
bif('xor',[Arg1,Arg2], error(badarg)) :-
  bif('and',[Arg1,Arg2], error(badarg)).

% 8.14 Short-Circuit Expressions -----------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#short-circuit-expressions
/** ----------------------------------------------------------------------------
 * Expr1 orelse Expr2
 * Expr1 andalso Expr2
 * are converted into case-of statements by core erlang
 */

% 8.15 List Operations ---------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#list-operations
/** ----------------------------------------------------------------------------
 * Expr1 ++ Expr2
 */
bif('++',[nil,Arg], Arg). % independently of whether or not Arg is a list
bif('++',[cons(X,Xs),Arg], cons(X,Tl)) :-
  bif('++',[Xs,Arg], Tl), % Tl might be anything but a error(badarg)
  dif(Tl,error(badarg)).
%
bif('++',[cons(_,Xs),Arg2], error(badarg)) :-
  bif('++',[Xs,Arg2], error(badarg)).
bif('++',[Arg1,_Arg2], error(badarg)) :-
  when(nonvar(Arg1), (Arg1 \= cons(_,_), Arg1 \= nil) ).

/** ----------------------------------------------------------------------------
 * Expr1 -- Expr2
 * Note that Expr1 and Expr2 must be proper lists.
 */
% [] -- []
bif('--',[nil,nil], nil).
% [] -- [Y|Ys]
bif('--',[nil,cons(_Y,Ys)], nil) :-
  bif('--',[nil,Ys], nil). % this checks if Ys is a proper list
% [X|Xs] -- []
bif('--',[cons(X,nil),nil], cons(X,nil)).
bif('--',[cons(X,Xs), nil], cons(X,cons(Z,Zs))) :-
  dif(Xs,nil), bif('--',[Xs,nil], cons(Z,Zs)).
% [X|Xs] -- [Y|Ys]
bif('--',[cons(X,Xs),cons(Y,Ys)], Zs) :-
  bif('==',[X,Y],lit(atom,Res)),
  list_sub_cont(Res,cons(X,Xs),Y,Rs),
  bif('--',[Rs,Ys], Zs).
%
list_sub_cont(true, cons(_X,Xs),_Y,Xs).
list_sub_cont(false,cons(X,nil),_Y,cons(X,nil)).
list_sub_cont(false,cons(X,Xs),Y,cons(X,cons(Z,Zs))) :-
  dif(Xs,nil), bif('--',[Xs,Y], cons(Z,Zs)).
%
bif('--',[cons(_X,Xs),nil], error(badarg)) :-
  bif('--',[Xs,nil], error(badarg)). % this checks if Xs is not a proper list
bif('--',[nil,cons(_Y,Ys)], error(badarg)) :-
  bif('--',[nil,Ys], error(badarg)). % this checks if Ys is not a proper list
bif('--',[cons(_X,Xs),cons(_Y,Ys)], error(badarg)) :-
  bif('--',[Xs,Ys], error(badarg)).
bif('--',[Arg1,Arg2], error(badarg)) :-
  var(Arg1),
  when(nonvar(Arg2), ( Arg2 \= cons(_,_), Arg2 \= nil ) ).
bif('--',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg1), ( Arg1 = cons(_,_) ; Arg1 = nil ),
  when(nonvar(Arg2), ( Arg2 \= cons(_,_), Arg2 \= nil ) ).
bif('--',[Arg1,Arg2], error(badarg)) :-
  var(Arg2),
  when(nonvar(Arg1), ( Arg1 \= cons(_,_), Arg1 \= nil ) ).
bif('--',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg2), ( Arg2 = cons(_,_) ; Arg2 = nil ),
  when(nonvar(Arg1), ( Arg1 \= cons(_,_), Arg1 \= nil ) ).
bif('--',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg1), ( Arg1 \= cons(_,_), Arg1 \= nil ),
  nonvar(Arg2), ( Arg2 \= cons(_,_), Arg2 \= nil ).

/** ----------------------------------------------------------------------------
 *  abs(Float) -> float()
 *  abs(Int) -> integer() >= 0
 *
 *    Types
 *      Int = integer()
 *
 *    Returns an integer or float that is the arithmetical absolute value of
 *    Float or Int, for example:
 *
 *    > abs(-3.33).
 *    3.33
 *    > abs(-3).
 *    3
 *
 *    Allowed in guard tests.
 */
bif('abs',[lit(int,V)], lit(int,V)) :-
  V #>=0.
bif('abs',[lit(int,V)], lit(int,A)) :-
  V #=< -1, A #= -1*V.
bif('abs',[lit(float,V)], lit(float,V)) :-
  { V >=0 }.
bif('abs',[lit(float,V)], lit(float,A)) :-
  { V < 0, A =:= -1*V }.
%
bif('abs',[Arg], error(badarg)) :-
  bif('+',[Arg], error(badarg)).

/** ----------------------------------------------------------------------------
 *  erlang:append_element(Tuple1, Term) -> Tuple2
 *
 *    Types
 *      Tuple1 = Tuple2 = tuple()
 *      Term = term()
 *
 *    Returns a new tuple that has one element more than Tuple1, and
 *    contains the elements in Tuple1 followed by Term as the last
 *    element. Semantically equivalent to
 *    list_to_tuple(tuple_to_list(Tuple1) ++ [Term]), but much
 *    faster. Example:
 *
 *    > erlang:append_element({one, two}, three).
 *    {one,two,three}
 */
bif('append_element',[tuple(T1),E], tuple(T2)) :-
  append(T1,[E],T2).
%
bif('append_element',[Arg1,_Arg2], error(badarg)) :-
  when(nonvar(Arg1), Arg1 \= tuple(_) ).

/** ----------------------------------------------------------------------------
 *  apply(Fun, Args) -> term()
 *
 *    Types
 *      Fun = function()
 *      Args = [term()]
 *
 *    Calls a fun, passing the elements in Args as arguments.

 *    If the number of elements in the arguments are known at compile
 *    time, the call is better written as bif(Arg1, Arg2, ... ArgN).
 */
% TODO: check thrown error
bif('apply',[FName,Args], Exp) :-
  eval(apply(FName,Args),[],Exp).

/** ----------------------------------------------------------------------------
 *  apply(Module, Function, Args) -> term()
 *
 *    Types
 *      Module = module()
 *      Function = atom()
 *      Args = [term()]
 *
 *    Returns the result of applying Function in Module to Args. The
 *    applied function must be exported from Module. The arity of the
 *    function is the length of Args. Example:
 *
 *    > apply(lists, reverse, [[a, b, c]]).
 *    [c,b,a]
 *    > apply(erlang, atom_to_list, ['Erlang']).
 *    "Erlang"
 *
 *    If the number of arguments are known at compile time, the call is
 *    better written as Module:Function(Arg1, Arg2, ..., ArgN).
 *
 *    Failure: error_handler:undefined_function/3 is called if the
 *    applied function is not exported. The error handler can be redefined
 *    (see process_flag/2). If error_handler is undefined, or if the user
 *    has redefined the default error_handler so the replacement module
 *    is undefined, an error with reason undef is generated.
 */
% TODO: check thrown error
bif('apply',[_Mod,FName,Args], Exp) :-
  % TODO: Pass module here
  eval(apply(FName,Args),[],Exp).

/** ----------------------------------------------------------------------------
 *  atom_to_list(Atom) -> string()
 *
 *    Types
 *      Atom = atom()
 *
 *    Returns a string corresponding to the text representation of Atom, for
 *    example:
 *
 *    > atom_to_list('Erlang').
 *    "Erlang"
 */
bif('atom_to_list',[lit(atom,A)], L) :-
  when(nonvar(A), ( atom_codes(A,C), plst2cons(C,L) ) ).
%
plst2cons([],nil).
plst2cons([Hd|PTl],cons(Hd,Tl)) :-
  plst2cons(PTl,Tl).
%
bif('atom_to_list',[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= lit(atom,_) ).

/** ----------------------------------------------------------------------------
 *  ceil(Number) -> integer()
 *
 *    Types
 *    Number = number()
 *
 *    Returns the smallest integer not less than Number. For example:
 *
 *    > ceil(5.5).
 *    6
 *
 *    Allowed in guard tests.
 %
 *    Test cases:
 *    > ceil(1.0).
 *    1
 *    > ceil(1.1).
 *    2
 *    > ceil(-1.0).
 *    -1
 *    > ceil(-1.1).
 *    -1
 *
 *    bif('ceil',[lit(float,1.0)], lit(int,C)).
 *    bif('ceil',[lit(float,1.1)], lit(int,C)).
 *    bif('ceil',[lit(float,-1.0)], lit(int,C)).
 *    bif('ceil',[lit(float,-1.1)], lit(int,C)).
 *
 *  https://proofwiki.org/wiki/Ceiling_of_Negative_equals_Negative_of_Floor
 */
bif('ceil',[lit(int,N)], lit(int,N)).
bif('ceil',[lit(float,N)], lit(int,C)) :-
  { N >= 0, C >= N }, C #>= 0,
  when(nonvar(N), C is ceil(N)).
bif('ceil',[lit(float,N)], lit(int,C)) :-
  { N < 0, C >= N, M = -N }, C #=< -1, C #= -V,
  bif('floor',[lit(float,M)], lit(int,V)).
%
bif('ceil',[Arg], error(badarg)) :-
  bif('+',[Arg], error(badarg)).

 /** ----------------------------------------------------------------------------
 *  erlang:delete_element(Index, Tuple1) -> Tuple2
 *
 *    Types
 *      Index = integer() >= 1
 *       1..tuple_size(Tuple1)
 *      Tuple1 = Tuple2 = tuple()
 *
 *    Returns a new tuple with element at Index removed from tuple
 *    Tuple1, for example:
 *
 *    > erlang:delete_element(2, {one, two, three}).
 *    {one,three}
 */
bif('delete_element',[lit(int,N),tuple(X)], tuple(Y)) :-
  when(nonvar(X), ( length(X,S), N #>= 1, N #=< S, del_elem(N,X,Y) ) ).
bif('delete_element',[lit(int,N),tuple(X)], error(badarg)) :-
  N #>= 1, when(nonvar(X), ( length(X,S), N #> S ) ).
bif('delete_element',[lit(int,N),_Arg2], error(badarg)) :-
  N #=< 0.
bif('delete_element',[lit(int,N),Arg2], error(badarg)) :-
  N #>= 1, when(nonvar(Arg2), Arg2 \= tuple(_) ).

del_elem(N,L1,L2) :-
  del_elem_(N,1,L1,L2).

del_elem_(N,N,[_|T1],T1).
del_elem_(N,M,[H|T1],[H|T2]) :-
  N \== M,
  L is M+1,
  del_elem_(N,L,T1,T2).
%
bif('delete_element',[Arg1,Arg2], error(badarg)) :-
  var(Arg1),
  when(nonvar(Arg2), Arg2 \= tuple(_) ).
bif('delete_element',[Arg1,Arg2], error(badarg)) :-
  var(Arg1), Arg1 = lit(int,_),
  when(nonvar(Arg2), Arg2 \= tuple(_) ).
bif('delete_element',[Arg1,Arg2], error(badarg)) :-
  var(Arg2),
  when(nonvar(Arg1), Arg1 \= lit(int,_) ).
bif('delete_element',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg2), Arg2 = tuple(_),
  when(nonvar(Arg1), Arg1 \= lit(int,_) ).
bif('delete_element',[Arg1,Arg2], error(badarg)) :-
  nonvar(Arg1), Arg1 \= lit(int,_),
  nonvar(Arg2), Arg2 \= tuple(_).

 /** ---------------------------------------------------------------------------
 *  element(N, Tuple) -> term()
 *
 *    Types
 *      N = integer() >= 1
 *       1..tuple_size(Tuple)
 *      Tuple = tuple()
 *
 *    Returns the Nth element (numbering from 1) of Tuple, for example:
 *
 *    > element(2, {a, b, c}).
 *    b
 *
 *  Allowed in guard tests.
 */
bif('element',[lit(int,N),tuple(X)], Y) :-
  when(nonvar(X), ( length(X,S), N #>= 1, N #=< S, elem(N,X,Y) ) ).
bif('element',[lit(int,N),tuple(X)], error(badarg)) :-
  N #>= 1, when(nonvar(X), ( length(X,S), N #> S ) ).
bif('element',[lit(int,N),_Arg2], error(badarg)) :-
  N #=< 0.
bif('element',[lit(int,N),Arg2], error(badarg)) :-
  N #>= 1, when(nonvar(Arg2), Arg2 \= tuple(_)).

elem(N,L,E) :-
  elem_(N,1,L,E).

elem_(N,N,[E|_],E).
elem_(N,M,[_|T],E) :-
  N \== M,
  L is M+1,
  elem_(N,L,T,E).
%
bif('element',[Arg1,Arg2], error(badarg)) :-
  bif('delete_element',[Arg1,Arg2], error(badarg)).

/** ----------------------------------------------------------------------------
 *  error(Reason) -> no_return()
 *
 *    Types
 *      Reason = term()
 *
 *    Stops the execution of the calling process with the reason Reason, where
 *    Reason is any term. The exit reason is {Reason, Where}, where Where is a
 *    list of the functions most recently called (the current function first).
 *    As evaluating this function causes the process to terminate, it has no
 *    return value. Example:
 *
 *      > catch error(foobar).
 *        {'EXIT',{foobar,[{shell,apply_fun,3,
 *                                [{file,"shell.erl"},{line,906}]},
 *                  {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,677}]},
 *                  {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,430}]},
 *                  {shell,exprs,7,[{file,"shell.erl"},{line,687}]},
 *                  {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
 *                  {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}
 */
bif('error',[Reason],error(Reason)).

/** ----------------------------------------------------------------------------
 *   error(Reason, Args) -> no_return()
 *
 *     Types
 *       Reason = term()
 *       Args = [term()]
 *
 *     Stops the execution of the calling process with the reason Reason, where
 *     Reason is any term. The exit reason is {Reason, Where}, where Where is a
 *     list of the functions most recently called (the current function first).
 *     Args is expected to be the list of arguments for the current function;
 *     in Beam it is used to provide the arguments for the current function in
 *     the term Where. As evaluating this function causes the process to
 *     terminate, it has no return value.
 */
% TODO: the 'error' in eval/3 should be extended to deal with this function
bif('error',[Reason,_Args],error(Reason)).

/** ----------------------------------------------------------------------------
 *  float(Number) -> float()
 *
 *    Types
 *      Number = number()
 *
 *    Returns a float by converting Number to a float, for example:
 *
 *    > float(55).
 *    55.0
 *
 *    Allowed in guard tests.
 *    Note
 *
 *    If used on the top level in a guard, it tests whether the argument is
 *    a floating point number; for clarity, use is_float/1 instead.
 *
 *    When float/1 is used in an expression in a guard, such as 'float(A) ==
 *    4.0', it converts a number as described earlier.
 */
bif('float',[lit(int,N)], lit(float,F)) :-
  { F =:= N }.
bif('float',[lit(float,N)], lit(float,N)).
%
bif('float',[Arg], error(badarg)) :-
  bif('+',[Arg], error(badarg)).

/** ----------------------------------------------------------------------------
 *  float_to_list(Float) -> string()
 *
 *  Types
 *    Float = float()
 *
 *  The same as float_to_list(Float,[{scientific,20}]).
 */

/** ----------------------------------------------------------------------------
 *  float_to_list(Float, Options) -> string()
 *
 *  Types
 *    Float = float()
 *    Options = [Option]
 *    Option =
 *       {decimals, Decimals :: 0..253} |
 *       {scientific, Decimals :: 0..249} |
 *       compact
 *
 *    Returns a string corresponding to the text representation of Float
 *    using fixed decimal point formatting.
 *
 *    Available options:
 *
 *      * If option decimals is specified, the returned value contains
 *        at most Decimals number of digits past the decimal point. If the
 *        number does not fit in the internal static buffer of 256 bytes, the
 *        function throws badarg.
 *
 *      * If option compact is specified, the trailing zeros at the end of the
 *        list are truncated. This option is only meaningful together with
 *        option decimals.
 *
 *      * If option scientific is specified, the float is formatted using
 *        scientific notation with Decimals digits of precision.
 *
 *      * If Options is [], the function behaves as float_to_list/1.
 *
 *    Examples:
 *
 *    > float_to_list(7.12, [{decimals, 4}]).
 *    "7.1200"
 *    > float_to_list(7.12, [{decimals, 4}, compact]).
 *    "7.12"
 */

/** ----------------------------------------------------------------------------
 *  floor(Number) -> integer()
 *
 *    Types
 *      Number = number()
 *
 *    Returns the largest integer not greater than Number. For example:
 *
 *    > floor(-10.5).
 *    -11
 *
 *    Allowed in guard tests.
 *
 *    Test cases:
 *    > floor(1.0).
 *    1
 *    > floor(1.1).
 *    1
 *    > floor(-1.0).
 *    -1
 *    > floor(-1.1).
 *    -2
 *
 *    bif('floor',[lit(float,1.0)], lit(int,C)).
 *    bif('floor',[lit(float,1.1)], lit(int,C)).
 *    bif('floor',[lit(float,-1.0)], lit(int,C)).
 *    bif('floor',[lit(float,-1.1)], lit(int,C)).
 *
 *  https://proofwiki.org/wiki/Floor_of_Negative_equals_Negative_of_Ceiling
 */
bif('floor',[lit(int,N)], lit(int,N)).
bif('floor',[lit(float,N)], lit(int,F)) :-
  { N >= 0, F =< N }, F #>= 0,
  when(nonvar(N), ( V is floor(N), F #= V ) ).
bif('floor',[lit(float,N)], lit(int,F)) :-
  { N < 0, F =< N, M = -N }, F #=< -1, F #= -V,
  bif('ceil',[lit(float,M)], lit(int,V)).
%
bif('floor',[Arg], error(badarg)) :-
  bif('+',[Arg], error(badarg)).

/** ----------------------------------------------------------------------------
 *  hd(List) -> term()
 *
 *  Types
 *    List = [term(), ...]
 *
 *  Returns the head of List, that is, the first element, for example:
 *
 *  > hd([1,2,3,4,5]).
 *  1
 *
 *  Allowed in guard tests.
 *
 *  Failure: badarg if List is the empty list [].
 */
bif('hd',[cons(H,_)], H).
%
bif('hd',[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= cons(_,_) ; Arg \= nil ) ).

/** ----------------------------------------------------------------------------
 *  erlang:insert_element(Index, Tuple1, Term) -> Tuple2
 *    Types
 *      Index = integer() >= 1
 *       1..tuple_size(Tuple1) + 1
 *      Tuple1 = Tuple2 = tuple()
 *      Term = term()
 *
 *    Returns a new tuple with element Term inserted at position Index in
 *    tuple Tuple1. All elements from position Index and upwards are
 *    pushed one step higher in the new tuple Tuple2. Example:
 *
 *    > erlang:insert_element(2, {one, two, three}, new).
 *    {one,new,two,three}
*/
bif('insert_element',[lit(int,N),tuple(X),E], tuple(Y)) :-
  when(nonvar(X), ( length(X,S), N #>= 1, N #=< S, ins_elem(N,X,E,Y) ) ).
bif('insert_element',[lit(int,N),tuple(X),_E], error(badarg)) :-
  N #>= 1, when(nonvar(X), ( length(X,S), N #> S ) ).
bif('insert_element',[lit(int,N),_Arg2,_E], error(badarg)) :-
  N #=< 0.
bif('insert_element',[lit(int,N),Arg2,_E], error(badarg)) :-
  N #>= 1, when(nonvar(Arg2), Arg2 \= tuple(_)).
bif('insert_element',[Arg1,_Arg2,_E], error(badarg)) :-
  when(nonvar(Arg1), Arg1 \= lit(int,_) ).

ins_elem(N,L1,E,L2) :-
  ins_elem_(N,1,L1,E,L2).

ins_elem_(N,N,T1,E,[E|T1]).
ins_elem_(N,M,[H|T1],E,[H|T2]) :-
  N \== M,
  L is M+1,
  ins_elem_(N,L,T1,E,T2).
%
bif('insert_element',[Arg1,Arg2,Arg2], error(badarg)) :-
  bif('delete_element',[Arg1,Arg2], error(badarg)).

/** ----------------------------------------------------------------------------
 *  integer_to_list(Integer) -> string()
 *
 *    Types
 *      Integer = integer()
 *
 *    Returns a string corresponding to the text representation of Integer,
 *    for example:
 *
 *    > integer_to_list(77).
 *    "77"
 */
bif('integer_to_list',[lit(int,N)], L) :-
  when(nonvar(N), ( atom_codes(N,C), plst2cons(C,L) ) ).
%
bif('integer_to_list',[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= lit(int,_) ).

/** ----------------------------------------------------------------------------
 *  integer_to_list(Integer, Base) -> string()
 *
 *    Types
 *      Integer = integer()
 *      Base = 2..36
 *
 *    Returns a string corresponding to the text representation of Integer
 *    in base Base, for example:
 *
 *    > integer_to_list(1023, 16).
 *    "3FF"
 */

/** ----------------------------------------------------------------------------
 *  is_atom(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is an atom, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif('is_atom',[lit(atom,_)], lit(atom,true)).
bif('is_atom',[Arg], lit(atom,false)) :-
  when(nonvar(Arg), Arg \= lit(atom,_) ).

/** ----------------------------------------------------------------------------
 *  is_float(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a floating point number, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif('is_float',[lit(float,_)], lit(atom,true)).
bif('is_float',[Arg], lit(atom,false)) :-
  when(nonvar(Arg), Arg \= lit(float,_) ).

/** ----------------------------------------------------------------------------
 *  is_function(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a fun, otherwise false.
 *
 *    Allowed in guard tests.
 */

/** ----------------------------------------------------------------------------
 *  is_function(Term, Arity) -> boolean()
 *
 *    Types
 *      Term = term()
 *      Arity = arity()
 *
 *    Returns true if Term is a fun that can be applied with Arity number
 *    of arguments, otherwise false.
 *
 *    Allowed in guard tests.
 */

/** ----------------------------------------------------------------------------
 *  is_integer(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is an integer, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif('is_integer',[lit(int,_)], lit(atom,true)).
bif('is_integer',[Arg], lit(atom,false)) :-
  when(nonvar(Arg), Arg \= lit(int,_) ).

/** ----------------------------------------------------------------------------
 *  is_list(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a list with zero or more elements, otherwise
 *    false.
 *
 *    Allowed in guard tests.
 */
bif('is_list',[nil], lit(atom,true)).
bif('is_list',[cons(_,_)], lit(atom,true)).
bif('is_list',[Arg], lit(atom,false)) :-
  when(nonvar(Arg), ( Arg \= cons(_,_), Arg \= nil ) ).

/** ----------------------------------------------------------------------------
 *  is_number(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is an integer or a floating point number.
 *    Otherwise returns false.
 *
 *    Allowed in guard tests.
 */
bif('is_number',[lit(int,_)], lit(atom,true)).
bif('is_number',[lit(float,_)], lit(atom,true)).
bif('is_number',[Arg], lit(atom,false)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) ) ).

/** ----------------------------------------------------------------------------
 *  is_tuple(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a tuple, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif('is_tuple',[tuple(_)], lit(atom,true)).
bif('is_tuple',[Arg], lit(atom,false)) :-
  when(nonvar(Arg), Arg \= tuple(_) ).

/** ----------------------------------------------------------------------------
 *  length(List) -> integer() >= 0
 *
 *    Types
 *      List = [term()]
 *
 *    Returns the length of List, for example:
 *
 *    > length([1,2,3,4,5,6,7,8,9]).
 *    9
 */
bif('length',[nil], lit(int,N)) :-
  N #= 0.
bif('length',[cons(_,Tl)], lit(int,M)) :-
  M #= N+1, M #>= 1,
  when(nonvar(Tl), bif('length',[Tl], lit(int,N))).
%
bif('length',[cons(_,Tl)], error(badarg)) :-
  when(nonvar(Tl), bif('length',[Tl], error(badarg))).
bif('length',[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= cons(_,_), Arg \= nil ) ).

/** ----------------------------------------------------------------------------
 *  max(Term1, Term2) -> Maximum
 *
 *    Types
 *      Term1 = Term2 = Maximum = term()
 *
 *    Returns the largest of Term1 and Term2. If the terms are equal, Term1
 *    is returned.
 */
bif('max',[Arg1,Arg2], Exp) :-
  bif('<',[Arg1,Arg2], lit(atom,Res)),
  max_cont(Res,[Arg1,Arg2],Exp).
%
max_cont(false,[Arg1,_Arg2],Arg1).
max_cont(true, [_Arg1,Arg2],Arg2).

/** ----------------------------------------------------------------------------
 *  min(Term1, Term2) -> Minimum
 *
 *    Types
 *      Term1 = Term2 = Minimum = term()
 *
 *    Returns the smallest of Term1 and Term2. If the terms are equal,
 *    Term1 is returned.
 */
bif('min',[Arg1,Arg2], Exp) :-
  bif('>',[Arg1,Arg2], lit(atom,Res)),
  min_cont(Res,[Arg1,Arg2],Exp).
%
min_cont(false,[Arg1,_Arg2],Arg1).
min_cont(true, [_Arg1,Arg2],Arg2).

/** ----------------------------------------------------------------------------
 *  erlang:nif_error(Reason) -> no_return()
 *
 *    Types
 *      Reason = term()
 *
 *    Works exactly like error/1, but Dialyzer thinks that this BIF will return
 *    an arbitrary term. When used in a stub function for a NIF to generate an
 *    exception when the NIF library is not loaded, Dialyzer does not generate
 *    false warnings.
 */
bif('nif_error',[Reason], Out) :-
  bif('error',[Reason], Out).

/** ----------------------------------------------------------------------------
 *  erlang:nif_error(Reason, Args) -> no_return()
 *
 *    Types
 *      Reason = term()
 *      Args = [term()]
 *
 *    Works exactly like error/2, but Dialyzer thinks that this BIF will return
 *    an arbitrary term. When used in a stub function for a NIF to generate an
 *    exception when the NIF
 */
bif('nif_error',[Reason,Args], Out) :-
  bif('error',[Reason,Args], Out).

/** ----------------------------------------------------------------------------
 *  round(Number) -> integer()
 *
 *    Types
 *      Number = number()
 *
 *    Returns an integer by rounding Number, for example:
 *
 *    > round(5.5).
 *    6
 *
 *    Allowed in guard tests.
 *
 *    Test cases:
 *    > round(3.0).
 *    3
 *    > round(3.5).
 *    4
 *    > round(3.4).
 *    3
 *    > round(-3.5).
 *    -4
 *    > round(-3.4).
 *    -3
 *
 *    bif('round',[lit(float,3.0)],lit(int,N)).
 *    bif('round',[lit(float,3.5)],lit(int,N)).
 *    bif('round',[lit(float,-3.5)],lit(int,N)).
 *    bif('round',[lit(float,-3.4)],lit(int,N)).
 */
bif('round',[lit(int,N)], lit(int,N)).
bif('round',[lit(float,R)], lit(int,N)) :-
  { R >= 0, N - R =< 0.5, R < N },
  when(nonvar(R), N is round(R)).
bif('round',[lit(float,R)], lit(int,N)) :-
  { R >= 0, N =< R, R - N < 0.5 },
  when(nonvar(R), N is round(R)).
bif('round',[lit(float,R)], lit(int,N)) :-
  { R < 0, P = -R }, N #= -S,
  bif('round',[lit(float,P)], lit(int,S)).
%
bif('round',[Arg], error(badarg)) :-
  bif('+',[Arg], error(badarg)).

/** ----------------------------------------------------------------------------
 *  setelement(Index, Tuple1, Value) -> Tuple2
 *
 *    Types
 *      Index = integer() >= 1
 *        1..tuple_size(Tuple1
 *      Tuple1 = Tuple2 = tuple()
 *      Value = term()
 *
 *    Returns a tuple that is a copy of argument Tuple1 with the element
 *    specified by integer argument Index (the first element is the element
 *    with index 1) replaced by argument Value, for example:
 *
 *    > setelement(2, {10, green, bottles}, red).
 *    {10,red,bottles}
 */


/** ----------------------------------------------------------------------------
 *  size(Item) -> integer() >= 0
 *
 *    Types
 *      Item = tuple() | binary()
 *
 *    Returns the number of elements in a tuple or the number of bytes in a
 *    binary or bitstring, for example:
 *
 *    > size({morni, mulle, bwange}).
 *    3
 *    > size(<<11, 22, 33>>).
 *    3
 *
 *    For bitstrings, the number of whole bytes is returned. That is, if the
 *    number of bits in the bitstring is not divisible by 8, the resulting
 *    number of bytes is rounded down.
 *
 *    Allowed in guard tests.
 *
 *    See also tuple_size/1, byte_size/1, and bit_size/1.
 */
bif('size',[tuple(T)], S) :-
  bif('tuple_size',[tuple(T)], S).
%
bif('size',[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= tuple(_) ).
%TODO: to add support for binary()
%
/** ----------------------------------------------------------------------------
 *  tl(List) -> term()
 *
 *    Types
 *      List = [term(), ...]
 *
 *    Returns the tail of List, that is, the list minus the first element, for
 *    example:
 *
 *    > tl([geesties, guilies, beasties]).
 *    [guilies, beasties]
 *
 *    Allowed in guard tests.
 *
 *    Failure: badarg if List is the empty list [].
 */
bif('tl',[cons(_Hd,Tl)], Tl).
%
bif('tl',[Arg], error(badarg)) :-
  bif('hd',[Arg], error(badarg)).

/** ----------------------------------------------------------------------------
 *  trunc(Number) -> integer()
 *
 *    Types
 *      Number = number()
 *
 *    Returns an integer by truncating Number, for example:
 *
 *    > trunc(5.5).
 *    5
 *
 *    Allowed in guard tests.
 */
bif('trunc',[lit(int,N)], lit(int,N)).
bif('trunc',[lit(float,N)], lit(int,I)) :-
  { N >= 0 }, I #>= 0,
  bif('floor',[lit(float,N)], lit(int,I)).
bif('trunc',[lit(float,N)], lit(int,I)) :-
  { N < 0 }, I #=< -1,
  bif('ceil',[lit(float,N)], lit(int,I)).
%
bif('trunc',[Arg], error(badarg)) :-
  bif('+',[Arg], error(badarg)).

/** ----------------------------------------------------------------------------
 *  tuple_size(Tuple) -> integer() >= 0
 *
 *    Types
 *      Tuple = tuple()
 *
 *    Returns an integer that is the number of elements in Tuple, for example:
 *
 *    > tuple_size({morni, mulle, bwange}).
 *    3
 *
 *    Allowed in guard tests.
 */
bif('tuple_size',[tuple(T)], lit(int,S)) :-
  S #>= 0,
  when(nonvar(T), length(T,S) ).
%
bif('tuple_size',[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= tuple(_) ).

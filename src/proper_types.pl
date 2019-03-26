:- use_module(library(clpfd)).
:- use_module(library(clpr)).

:- dynamic config/1.

start_size(X) :-
	(config(start_size(Y)) -> X=Y ; X is 1).
max_size(X) :-
	(config(max_size(Y)) -> X=Y ; X is 42).


:- multifile typedef/2.


% ------------------------------------------------------------------------------
typeof(X,T) :-
  T =.. [F|As],
  builtin_type_pred(F),
  length(As,L),
  proper_type_to_prolog(F,L,P),
  G =.. [P,X|As],
  call(G).


typeof(X,T) :-
  typedef(T,D),
  typeof(X,D).


% typedef(tree(_T),exactly(lit(atom,leaf))).
% typedef(tree(T),tuple([exactly(lit(atom,node)),T,tree(T),tree(T)])).


proper_type_to_prolog(integer,0,integer_) :- !.
proper_type_to_prolog(float,0,float_) :- !.
proper_type_to_prolog(number,0,number_) :- !.
proper_type_to_prolog(atom,0,atom_) :- !.
proper_type_to_prolog(F,_,F).

% TODO: use goal_expansion?

% ------------------------------------------------------------------------------
% integer(Value,Low,High)
integer(lit(int,X),Low,High) :-
  ((High==inf) ->
      ((Low==inf) -> X in inf..sup ; X #>= Low)
    ;
      ((Low==inf) -> X #=< High ; (X #>= Low, X #=< High))
  ).
% tests:
% integer(X,inf,inf).
% integer(X,inf,A).
% integer(X,inf,2).
% integer(X,A,inf).
% integer(X,2,inf).
% integer(X,2,3).
% integer(X,2,A).
% integer(X,A,2).


% ------------------------------------------------------------------------------
% integer(Float,Low,High)
float(lit(float,X),Low,High) :-
  ((High==inf) ->
      ((Low==inf) -> clpr:{X=_} ; clpr:{X >= Low})
    ;
      ((Low==inf) -> clpr:{X =< High} ; clpr:{X >= Low, X =< High})
  ).
% tests:
% float(X,inf,inf).
% float(X,inf,A).
% float(X,inf,2).
% float(X,A,inf).
% float(X,2,inf).
% float(X,2,3).
% float(X,2,A).
% float(X,A,2).


% ------------------------------------------------------------------------------
% atom_(Atom)
% atom(X) built-in [ISO]
% In both Erlang and Prolog strings are atoms http://erlang.org/doc/reference_manual/data_types.html#atom
atom_(lit(atom,X)) :-
  when((nonvar(X)), atom(X)).


% ------------------------------------------------------------------------------
% non_empty(Value,Type)
non_empty(V,T) :-
  T = list(_),
  !,
  V = cons(_,_),
  typeof(V,T).
non_empty(_,T) :-
  term_to_atom(nonempty(T),Atom),
  atom_concat('non_empty/2: cannot generate ', Atom,Exception),
  throw(Exception).

% ------------------------------------------------------------------------------
% list(Value,Type)
%list(nil,_).
%list(cons(X,Xs),T) :-
%  typeof(X,T),
%  list(Xs,T).

random_size(N) :-
	start_size(MinL),
	max_size(MaxL),
  repeat,
	random_between(MinL,MaxL,N).

list(L,T) :-
	random_size(N),
	list(L,T,N).

list(nil,_,0).
list(cons(X,Xs),T,N) :-
	N>0,
	N1 is N-1,
  typeof(X,T),
  list(Xs,T,N1).


% ------------------------------------------------------------------------------
% fixed_list(ValuesLst,TypesLst)
fixed_list(nil,nil).
fixed_list(cons(X,Xs),cons(T,Ts)) :-
  typeof(X,T),
  fixed_list(Xs,Ts).


% ------------------------------------------------------------------------------
% vector(Values,Length,Type)
vector(X, Len, Type) :-
  llength(X,Len),
  list(X,Type).
% vector/3 utility predicate
llength(nil,L) :- clpq:{L=0}.
llength(cons(_,Xs),L) :- clpq:{L1>=0, L=1+L1}, llength(Xs,L1).


% ------------------------------------------------------------------------------
% union(Values,TypesLst)
union(X,ListOfTypes) :-
  member(T,ListOfTypes),  % use nondet random member selection based on random_member/2 ?
  typeof(X,T).



% ------------------------------------------------------------------------------
% tuple(ValuesLst,TypesLst)
tuple(tuple(X),T) :- tuple_(X,T).

tuple_([],[]).
tuple_([E|Es],[T|Ts]) :-
  typeof(E,T),
  tuple_(Es,Ts).



% ------------------------------------------------------------------------------

loose_tuple(X,Type) :-
	random_size(N),
	loose_tuple(X,Type,N).

loose_tuple(X,Type,N) :-
  length(L,N),
  list_same_element(L,Type),
  tuple(X,L).

% loose_tuple/2 utility predicate
list_same_element([],_).
list_same_element([T|Xs],T) :-
  list_same_element(Xs,T).


% ------------------------------------------------------------------------------
%
exactly(X,Term) :-
  X = Term.

% ------------------------------------------------------------------------------
%
any(X) :- is_type(T), typeof(X,T).


% utility predicates for any/1 and other predicates

is_type(T) :- is_builtin_type(T).
is_type(T) :- is_userdef_type(T).

is_builtin_type(integer(_L,_H)).
is_builtin_type(float(_L,_H)).
% TODO: add clauses for    is_builtin_type(non_empty(T)) 
is_builtin_type(list(T)) :- is_type(T).
is_builtin_type(fixed_list(L)) :- is_type_list(L).
is_builtin_type(vector(_, T)) :- is_type(T).
is_builtin_type(union(L)) :- is_type_list(L).
is_builtin_type(tuple(L)) :- is_type_list(L).
is_builtin_type(loose_tuple(T)) :- is_type(T).
is_builtin_type(exactly(Term)) :- atom(Term).

is_userdef_type(T) :- typedef(T,_).


builtin_type_pred(P) :-
	member(P,[integer, float, list, fixed_list, vector, union,
		tuple, loose_tuple, exactly, non_empty]).
builtin_type_pred(P) :- alias_type_pred(P).


alias_type_pred(P) :-
	member(P,[non_neg_integer,pos_integer,neg_integer,
		range, number, non_neg_float, boolean, byte, char, string, term ]).

is_type_list([]).
is_type_list([T|Ts]) :-
  is_type(T),
  is_type_list(Ts).


%%------------------------------------------------------------------------------
% Type aliases
%%------------------------------------------------------------------------------

integer_(X) :- integer(X,inf, inf).
non_neg_integer(X) :- integer(X,0, inf).
pos_integer(X) :- integer(X,1, inf).
neg_integer(X) :- integer(X,inf, -1).
range(X,Low, High) :- integer(X,Low, High).
float_(X) :- float(X,inf,inf).
non_neg_float(X) :- float(X,0,inf).
number_(X) :- union(X,[integer_,float_]).
boolean(X) :- union(X,[exactly('false'),exactly('true')]).
byte(X) :- integer(X,0, 255).
% char()  dec:1114111    hex:10ffff
char(X) :- integer(X,0, 1114111).
list(X) :- is_type(T), list(X,T).
tuple(X) :- is_type_list(L), tuple(X,L).
string(X) :- list(X,char).
term(X) :- any(X).

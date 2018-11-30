:- use_module(library(clpfd)).
:- use_module(library(clpr)).

%typeof(X,integer) :- integer(X).
%typeof(X,integer(2,5)) :- integer(X,2,5).

% general form
typeof(X,T) :- T =.. [F|As], length(As,L),
              proper_function_to_prolog_predicate(F,L,P),
              T1 =.. [P,X|As], T1.

% proper_function_to_prolog_predicate(F,P)
proper_function_to_prolog_predicate(integer,0,integer_) :- !.
proper_function_to_prolog_predicate(float,0,float_) :- !.
proper_function_to_prolog_predicate(number,0,number_) :- !.
proper_function_to_prolog_predicate(atom,0,atom_) :- !.
proper_function_to_prolog_predicate(F,_,F).

% TODO: use goal goal_expansion?

% X #>= inf gives error  ,  X in L..H requires inf/sup or integer for extremes


% TODO: integer(lit(int,X),Low,High) :-  ?????? ... everywhere
integer(X,Low,High) :-
  ((High==inf) ->
      ((Low==inf) -> X in inf..sup ; X #>= Low)
    ;
      ((Low==inf) -> X #=< High ; (X #>= Low, X #=< High))
  ).


% % tests

% integer(X,inf,inf).
% integer(X,inf,A).
% integer(X,inf,2).
% integer(X,A,inf).
% integer(X,2,inf).
% integer(X,2,3).
% integer(X,2,A).
% integer(X,A,2).


% ?- integer(X,inf,inf).
% X in inf..sup.
% ?- integer(X,inf,A).
% A#>=X.
% ?- integer(X,inf,2).
% X in inf..2.
% ?- integer(X,A,inf).
% X#>=A.
% ?- integer(X,2,inf).
% X in 2..sup.
% ?- integer(X,2,3).
% X in 2..3.
% ?- integer(X,2,A).
% X in 2..sup,
% A#>=X,
% A in 2..sup.
% ?- integer(X,A,2).
% X in inf..2,
% X#>=A,
% A in inf..2.


float(X,Low,High) :-
  ((High==inf) ->
      ((Low==inf) -> clpr:{X=_} ; clpr:{X >= Low})
    ;
      ((Low==inf) -> clpr:{X =< High} ; clpr:{X >= Low, X =< High})
  ).

% tests
% float(X,inf,inf).
% float(X,inf,A).
% float(X,inf,2).
% float(X,A,inf).
% float(X,2,inf).
% float(X,2,3).
% float(X,2,A).
% float(X,A,2).



% atom(X) built-in [ISO]
% In both Erlang and Prolog strings are atoms http://erlang.org/doc/reference_manual/data_types.html#atom
atom_(X) :- when((nonvar(X)), atom(X)).  %  X =.. [_F]


list([],_).
list([X|Xs],T) :-  typeof(X,T),  list(Xs,T).

fixed_list([],[]).
fixed_list([X|Xs],[T|Ts]) :-  typeof(X,T),   fixed_list(Xs,Ts).

vector(X, Len, Type) :- length(X,Len), list(X,Type).


union(X,ListOfTypes) :-
  member(T,ListOfTypes),  % use nondet random member selection based on random_member/2 ?
  typeof(X,T).



% {a,b,c} =.. L.
% L = [{},  (a, b, c)].
%
% ?- (a,b,c) =.. L.
% L = [',', a,  (b, c)].


tuple(X,ListOfTypes) :- list_types(L,ListOfTypes), list_to_tuple(L,X).
list_to_tuple([],{}).
list_to_tuple([X|Xs],T):-  dump:list_to_conj([X|Xs],C), T =.. [{},C].

loose_tuple(X,Type) :- list(L,Type), list_to_tuple(L,X).

exactly(X,Term) :- atom(Term), X=Term. % cfr X==Term

any(X) :- is_type(T), typeof(X,T).

is_type(integer(_L,_H)).
is_type(float(_L,_H)).
is_type(list(T)) :- is_type(T).
is_type(fixed_list(L)) :- is_type_list(L).
is_type(vector(_, T)) :- is_type(T).
is_type(union(L)) :- is_type_list(L).
is_type(tuple(L)) :- is_type_list(L).
is_type(loose_tuple(T)) :- is_type(T).
is_type(exactly(Term)) :- atom(Term).
%is_type(T) :- is_alias(T).


is_type_list([]).
is_type_list([T|Ts]) :- is_type(T), is_type_list(Ts).



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

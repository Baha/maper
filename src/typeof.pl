:- use_module(library(clpfd)).
:- use_module(library(clpr)).
:- multifile typedef/2.

:- dynamic min_size/1, max_size/1.
min_size(0).  % was X is 1
max_size(42).

:- dynamic config/1.
unset_config(X) :-
  X =.. [F|As],
  length(As,N),
  length(DontCare,N),
  X1 =.. [F|DontCare],
  retractall(config(X1)).

set_config(max_size(X)) :-
  !,
  retractall(max_size(_)),
  assertz(max_size(X)).
set_config(min_size(X)) :-
  !,
  retractall(min_size(_)),
  assertz(min_size(X)).
set_config(C) :-
  unset_config(C),
  assertz(config(C)).
% :- set_config(random_tuple).

show_config :-
  findall(X, (config(X),write(X),nl), _).

%% -----------------------------------------------------------------------------
%% X is of type T
typeof(X,T) :-
  member(T,[
    integer,non_neg_integer,pos_integer,neg_integer,range,
    float,non_neg_float,number,boolean,byte,atom,
    integer(_,_),float(_,_)]),
	!,
	{S=0},
	typeof_(X,T,S).

typeof(X,T) :-
	config(random_size(any)),
	!,
	min_size(StartS),
	max_size(MaxS),
	N is MaxS-StartS+1,
	repeat(N),
	random_size(S),
	typeof_(X,T,S).

typeof(X,T) :-
	config(random_size(all)),
	!,
	min_size(StartS),
	max_size(MaxS),
	random_between_all2(StartS,MaxS,S),
	typeof_(X,T,S).

typeof(X,T) :-
	size(S),
	typeof_(X,T,S).


% ------------------------------------------------------------------------------
% X is of type T and has size S
typeof(X,T,S) :-
	typeof_(X,T,S).

typeof_(X,T,S) :-
    T =.. [F|As],
    builtin_type_pred(F),
    length(As,L),
    proper_type_to_prolog(F,L,P),
		append(As,[S],Args),
		G =.. [P,X|Args],
    call(G).


typeof_(X,T,S) :-
  typedef_union(T,D),
  typeof_(X,D,S).
%
typedef_union(T,union(L)) :- findall(D,typedef(T,D),L), L \==[].

%
sizeof(X,S) :-
  {S=0},
  when(nonvar(X), ( X = lit(_,_) ; atomic(X) ) ).
sizeof(X,S) :-
  {S1>=0, S=1+S1},
  when(nonvar(X), (
    compound(X), X \= lit(_,_), X =.. [_|Args], sizeof_list(Args,S1) )
  ).


sizeof_list([],S) :- {S=0}.
sizeof_list([X|Xs],S) :-
							% ({S1=0};{S1>=1}),
							{ S1>=0, S2>=0, S=S1+S2},
              sizeof(X,S1),
              sizeof_list(Xs,S2).


proper_type_to_prolog(integer,0,integer_) :- !.
proper_type_to_prolog(float,0,float_) :- !.
proper_type_to_prolog(number,0,number_) :- !.
proper_type_to_prolog(atom,0,atom_) :- !.
proper_type_to_prolog(list,1,list_) :- !.
proper_type_to_prolog(union,1,union_) :- !.
proper_type_to_prolog(weighted_union,1,weighted_union_) :- !.
proper_type_to_prolog(F,_,F).

% TODO: use goal_expansion?

% ------------------------------------------------------------------------------
% integer(Value,Low,High)
integer(lit(int,X),Low,High) :-
	{S=0},
	integer(lit(int,X),Low,High,S).

integer(lit(int,X),Low,High,S) :-
	{S=0},
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
	{S=0},
	float(lit(float,X),Low,High,S).

float(lit(float,X),Low,High,S) :-
	{S=0},
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
% non_empty(V,T) :-
%   T = list(_),
%   !,
%   V = cons(_,_),
%   typeof(V,T).
% non_empty(_,T) :-
%   term_to_atom(nonempty(T),Atom),
%   atom_concat('non_empty/2: cannot generate ', Atom,Exception),
%   throw(Exception).

non_empty(X,T,S) :-
	{S>=1},
	typeof(X,T,S).

% ------------------------------------------------------------------------------
% list(Value,Type)
%list(nil,_).
%list(cons(X,Xs),T) :-
%  typeof(X,T),
%  list(Xs,T).

random_size(N) :-
	min_size(MinL),
	max_size(MaxL),
	random_between(MinL,MaxL,N).



% like random_between but produces all answers when backtracking
random_between_all(Min,Max,N) :-
	findall(X,between(Min,Max,X),L),
	random_select_all(N,L).

random_select_all(N,L) :-
	random_select(E,L,Rest),
	(N=E; random_select_all(N,Rest) ).


% like random_between_all but without explicit list generation
random_between_all2(MinL,MaxL,N) :-
 	{N>=MinL, N=<MaxL},
 	random_between(MinL,MaxL,X),
 	(
		{N=X}
	;
		({N>=X+1}, random_between_all2(MinL,MaxL,N))
		;
		({N=<X-1}, random_between_all2(MinL,MaxL,N))
	).



% like random_member but produces all answers when backtracking
member_random(X,L) :-
	random_select(Y,L,Rest),
	(X=Y; member_random(X,Rest)).


member_random_freq(X,[{_F,X}]) :- !.
member_random_freq(X,ListOfFreqTypes) :-
	foldl(freq_sum,ListOfFreqTypes,{0,_},{FreqSum,_}),
	random_between(1,FreqSum,R),
	member_freq({_,X},ListOfFreqTypes,R).

freq_sum({A,_}, {B,_}, {S,_}) :- S is A + B.

member_freq({F,X},[{F1,X1}| _ ],R) :-
	R =< F1, !, {F,X} = {F1,X1}.
member_freq({F,X},[{F1,_}| ListOfFreqTypes],R) :-
		R > F1, !,
		R1 is R-F1,
		member_freq({F,X},ListOfFreqTypes,R1).



size(N) :-
  min_size(MinL),
  max_size(MaxL),
  % MinL #=< N, N #=< MaxL
  {MinL =< N, N =< MaxL}.


% list(nil,_).
% list(cons(X,Xs),T) :-
%     typeof(X,T),
%     list(Xs,T).

%%%%
% list(L,T) :- {S>=0}, list(L,T,S)

list_sized(L,T) :-
  	random_size(S),
  	list(L,T,S).


list_(nil,_,S) :- {S=0}.
list_(cons(X,Xs),T,S) :-
	{S1>=0, S2>=0, S=1+S1+S2},
  typeof(X,T,S1),
  list_(Xs,T,S2).


% ------------------------------------------------------------------------------
% fixed_list(ValuesLst,TypesLst)
fixed_list(nil,nil,S) :- {S=0}.
fixed_list(cons(X,Xs),cons(T,Ts),S) :-
	{S1>=0, S2>=0, S=1+S1+S2},
  typeof(X,T,S1),
  fixed_list(Xs,Ts,S2).


% ------------------------------------------------------------------------------
% vector(Values,Length,Type)
vector(X, Len, Type) :-
  llength(X,Len),
  list(X,Type).
% vector/3 utility predicate
llength(nil,L) :- clpq:{L=0}.
llength(cons(_,Xs),L) :- clpq:{L1>=0, L=1+L1}, llength(Xs,L1).


% ------------------------------------------------------------------------------
% union_(Values,TypesLst,Size)
% note PropEr ignores size of union
union_(X,ListOfTypes,S) :-
	ListOfTypes \== [],
	( (config(random_union) ) ->
			member_random(T,ListOfTypes)
		;
  		member(T,ListOfTypes)
	),
  typeof_(X,T,S).

	% ------------------------------------------------------------------------------
	% weighted_union_(Values,FreqTypesLst,Size)
	weighted_union_(X,ListOfFreqTypes,S) :-
		ListOfFreqTypes \== [],
		( (config(random_union) ) ->
				member_random_freq(T,ListOfFreqTypes)
			;
	  		member({_,T},ListOfFreqTypes)
		),
	  typeof_(X,T,S).


% ------------------------------------------------------------------------------
% tuple(ValuesLst,TypesLst)

% tuple(tuple(X),T) :-
% 	write('TUPLE/2'), nl,
% 	sizeof(X,S),
% 	tuple(tuple(X),T,S).

tuple(tuple(X),T,S) :-
	lists_same_length(X,T,_L),
	{S = 1+S1, S1>=0},
	tuple_(X,T,S1).

tuple_([],[],S) :- {S=0}.
tuple_([E|Es],[T|Ts],S) :-
	{S = S1+S2, S1>=0, S2>=0},
	(config(random_tuple)  ->
					(
				length([E|Es],L),
				random_between(1,L,Pos),
				nth1(Pos,[E|Es],E1,Es1),
				nth1(Pos,[T|Ts],T1,Ts1)
					)
			;
			(E1=E, T1=T, Es1=Es, Ts1=Ts)
	),
  typeof(E1,T1,S1),
  tuple_(Es1,Ts1,S2).


% lists_same_length(X,T) :- length(T,L),	length(X,L).
lists_same_length([],[],0).
lists_same_length([_|X],[_|Y],L) :- {L=L1+1}, lists_same_length(X,Y, L1).

% ------------------------------------------------------------------------------

loose_tuple(X,Type) :-
	random_size(S),
	loose_tuple(X,Type,S).

loose_tuple(X,Type,S) :-
  length(L,S),
  list_same_element(L,Type),
  tuple(X,L,S).

% loose_tuple/2 utility predicate
list_same_element([],_).
list_same_element([T|Xs],T) :-
  list_same_element(Xs,T).


% ------------------------------------------------------------------------------
%
exactly(X,Term,S) :-
	{S>=0},
	sizeof(Term,S),
  X = Term.

% ------------------------------------------------------------------------------
%
any(X,S) :- is_type(T), typeof(X,T,S).


% utility predicates for any/1 and other predicates

is_type(T) :- is_builtin_type(T).
is_type(T) :- is_userdef_type(T).

is_builtin_type(T) :- T =.. [F|_], builtin_type_pred(F).

% is_builtin_type(integer(_L,_H)).
% is_builtin_type(float(_L,_H)).
% % TODO: add clauses for    is_builtin_type(non_empty(T))
% is_builtin_type(list(T)) :- is_type(T).
% is_builtin_type(fixed_list(L)) :- is_type_list(L).
% is_builtin_type(vector(_, T)) :- is_type(T).
% is_builtin_type(union(L)) :- is_type_list(L).
% is_builtin_type(tuple(L)) :- is_type_list(L).
% is_builtin_type(loose_tuple(T)) :- is_type(T).
% is_builtin_type(exactly(Term)) :- atom(Term).

is_userdef_type(T) :- typedef(T,_).


builtin_type_pred(P) :-
	member(P,[integer, float, list, fixed_list, vector, union, weighted_union,
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

integer_(X,S) :- {S=0}, integer(X,inf, inf,S).
non_neg_integer(X,S) :- {S=0}, integer(X,0, inf,S).
pos_integer(X,S) :- {S=0}, integer(X,1, inf,S).
neg_integer(X,S) :- {S=0}, integer(X,inf, -1,S).
range(X,Low, High,S) :- {S=0}, integer(X,Low, High,S).
float_(X,S) :- {S=0}, float(X,inf,inf,S).
non_neg_float(X,S) :- {S=0}, float(X,0,inf,S).
number_(X,S) :- {S=0}, union_(X,[integer_,float_],S).
boolean(X,S) :- {S=0}, union_(X,[exactly('false'),exactly('true')],S).
byte(X,S) :- {S=0}, integer(X,0, 255,S).
%%%% char()  dec:1114111    hex:10ffff

%char(X) :- integer(X,0, 1114111).
list(X,S) :- is_type(T), list_(X,T,S).
%tuple(X) :- is_type_list(L), tuple(X,L).
%string(X) :- list(X,char).
%term(X) :- any(X).

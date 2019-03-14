:- use_module(library(clpr)).  
:- use_module(library(clpfd)).	

:- consult('./proper_types.pl').
:- consult('./eval.pl').

:- working_directory(CWD, CWD),
   working_directory(CWD, CWD).
 

:- dynamic config/1.


% lit(int,X)

int_sup(Sup) :- Exp is 12, Sup is 10**Exp.
int_inf(Inf) :- int_sup(Sup), Inf is -Sup.

int_sup(X,SupX) :- fd_sup(X,S), 
		(S==sup -> 
			int_sup(SupX) 
			; 
			SupX=S
		).

int_inf(X,InfX) :- fd_inf(X,I), 
		(I==inf -> 
			int_inf(InfX) 
			; 
			InfX=I
		).


rand_int(X) :- 
		int_inf(X,Inf),
		int_sup(X,Sup),
		random_between(Inf,Sup,X).






% lit(float,X)

float_sup(Sup) :- Exp is 12, Sup is (10**Exp)+0.000000000001.
float_inf(Inf) :- float_sup(Sup), Inf is -Sup.

float_sup(X,SupX) :-  
		(sup(X,S) -> 
			SupX=S
			; 
			float_sup(SupX) 			
		).

float_inf(X,InfX) :-  
		(inf(X,I) -> 
			InfX=I 
			; 
			float_inf(InfX)
		).


rand_float(X) :- 
		float_inf(X,Inf),
		float_sup(X,Sup),
		
		Len is Sup-Inf,
		random(R),
		X is Inf+R*Len.

%		random(Inf,Sup,X).
% SWI doc says 
% random/3 deprecated 		
% Please use random/1 for generating a random float ... but 
% random/1 Binds R to a new random float in the open interval (0.0,1.0). 



% ------------------------------------------------------------------------------
% list(Value,Length,Type)  
% TODO: move to proper_types ?
% list(Value,Length,Type) :- 	vector(Value,Length,Type)






generate_test_cases(G) :-
	retractall(config(_)),
	assertz(config(listlength_inf(3))),
	assertz(config(listlength_sup(10))),
	call(G),
	G =.. [_,Arg],
	!,
	conj_to_list(Arg,ArgL),
	maplist(rand_elem,ArgL),
%	maplist(write_elem,ArgL)
	length(ArgL,N),
	findall(_,(nth1(I,ArgL,X), write_elem(X), (I<N -> write(',') ; true)),_)
	.




%%%% Random element generation

rand_elem(nil).
rand_elem(cons(X,L)) :-  
	rand_elem(X),
	rand_elem(L). 

rand_elem(lit(int,X)) :- rand_int(X).
rand_elem(lit(float,X)) :- rand_float(X).

%%%% Write element 

write_elem(lit(int,X)) :-  write(X).
write_elem(lit(float,X)) :-  write(X).

write_elem(nil) :- write('[]').
write_elem(cons(X,L)) :- write('['), write_elem_(cons(X,L)) ,write(']').

write_elem_(cons(X,nil)) :- write_elem(X).
write_elem_(cons(X,cons(Y,L))) :-  write_elem(X), write(','), write_elem_(cons(Y,L)).






%%%%% Random element generation with explicit type specification 
%
%rand_elem_type(nil,_T).
%rand_elem_type(cons(X,L),list(T)) :-  
%	rand_elem_type(X,T),
%	rand_elem_type(L,list(T)). 
%
%
%% rand_elem_type(lit(int,X),integer) :- rand_int(X).
%
%rand_elem_type(X,T) :- 
%	functor(T,F,_),
%	member(F,[integer,float]),
%	typeof(X,T),
%	rand_elem_type_(X,F)
%	.
%
%rand_elem_type_(lit(int,X),integer) :- rand_int(X).
%rand_elem_type_(lit(float,X),float) :- rand_float(X).
%
%
%%%%% Write element with explicit type specification 
%
%write_elem_type(lit(_,X),T) :- functor(T,F,_), member(F,[integer,float]), write(X).
%
%write_elem_type(nil,_) :- write('[]').
%write_elem_type(cons(X,L),T) :- write('['), write_elem_type_(cons(X,L),T) ,write(']').
%
%write_elem_type_(cons(X,nil),list(T)) :- write_elem_type(X,T).
%write_elem_type_(cons(X,cons(Y,L)),list(T)) :-  write_elem_type(X,T), write(','), write_elem_type_(cons(Y,L),list(T)).
%
%
%
%% TESTS
%% rand_elem_type(X,list(integer)).
%% rand_elem_type(X,list(integer(10,200))).
%% rand_elem_type(X,list(list(integer(10,200)))).
%%
%% rand_elem_type(X,list(float)).
%% rand_elem_type(X,list(float(10,200))).
%% rand_elem_type(X,list(list(float(10,200)))).
%
%
%% T=list(list(float)), rand_elem_type(X,T), write_elem_type(X,T).
%
%
%
%%%%%%%%%%%%  PLAYGROUND
%
%
%rand_list_int_sort_randset(N,S,L) :- 
%	S1 is 2*S+1, 
%	randset(N,S1,L1), % slow when S1 is large
%	D is S+1,
%	maplist(subtract_(D),L1,L).
%
%subtract_(A,X,X1) :- X1 is X-A.	
%
%rand_list_int(N,S,L) :- 
%	length(L1,N),
%	S1 is -S,
%	maplist(random_between_(S1,S),L1,L).
%
%rand_list_int_sort(N,S,L) :- 
%	rand_list_int(N,S,L2),
%	sort(L2,L).
%
%	
%random_between_(Inf,Sup,_,R) :- random_between(Inf,Sup,R).





%%%% UTILITIES %%%%%%

run :-
	prolog_flag(argv,ArgV),
	ArgV = [Example,Prop,NS],
	consult(Example),
	CallA =.. [Prop,_],
	atom_number(NS,N),
	% gtime().
	findall(_,(repeat(N), generate_test_cases(CallA),nl),_).



% conj_to_list(S,L): S a conjunction of the form (A1,...,An), and L is the list [A1,...,An].
conj_to_list(X,[]) :-
  X==true,
  !.
conj_to_list(B,L) :-
  ( nonvar(B), functor(B,',',_) ->
    ( B = (B1,B2), L=[B1|H], conj_to_list(B2,H) )
  ;
    L=[B]
  ).


% Code borrowed from Richard O'Keefe, The Craft of Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% repeat a number of times
repeat(N) :-
             integer(N), % type check
             N>0,        % value check 
             repeat1(N).

repeat1(1) :- !. % the cut will prevent the search for other solutions of repeat1(1)
                 % this clause does not fit any N superior to 1
repeat1(_).  % first, we succeed with N
repeat1(N) :- M is N-1,    % when retried because of the fail from above (go1 below...)
                           % we call for a new invocation with a new arg (decreased by 1)
              repeat1(M).    % 


gtime(G) :-
	statistics(runtime,[T1,_]),
	G,
	statistics(runtime,[T2,_]),
	Time is T2-T1,
	write('Time: '), write(Time), write(' ms'), nl, flush_output.



:- use_module(library(clpr)).
:- use_module(library(clpfd)).
:- use_module('./eval.pl').
:- consult('./typeof.pl').

:- working_directory(CWD, CWD),
   working_directory(CWD, CWD).

% ------------------------------------------------------------------------------
% lit(int,X)
% ------------------------------------------------------------------------------
:- set_config(int_sup(1000)).
:- set_config(int_inf(-1000)).

int_sup(X,SupX) :- fd_sup(X,S),
		(S==sup ->
			config(int_sup(SupX))
			;
			SupX=S
		).

int_inf(X,InfX) :- fd_inf(X,I),
		(I==inf ->
			config(int_inf(InfX))
			;
			InfX=I
		).


rand_int(X) :-
		int_inf(X,Inf),
		int_sup(X,Sup),
		random_between(Inf,Sup,X).



% ------------------------------------------------------------------------------
% lit(float,X)
% ------------------------------------------------------------------------------

float_sup(Sup) :- int_sup(Sup1), Sup is Sup1 + 0.000000000001.
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
% SWIPL doc says
% random/3 deprecated
% Please use random/1 for generating a random float ... but
% random/1 Binds R to a new random float in the open interval (0.0,1.0).


% generate, instantiate and write N instances of G
generate_test_cases(G,N) :-
  M is sqrt(N),
  C is ceil(M),
  once(
      findnsols(N, _,
              (   call(G), % eval & typeof
                  %write(user_error,'ciao'),
                	G =.. [_|ArgL], length(ArgL,AL),
                  between(1,C,_),
                	maplist(rand_elem,ArgL),
                	findall(_,(nth1(I,ArgL,X), write_elem(X), (I<AL -> write(',') ; true)),_)
                  , nl
            )
        , _ )
  ).





%%%% Random element generation

rand_elem(X) :- ground(X), !.
rand_elem(lit(int,X)) :- rand_int(X).
rand_elem(lit(float,X)) :- rand_float(X).
%rand_elem(lit(atom,X)) :-  TODO ++++

rand_elem(nil).
rand_elem(cons(X,L)) :-
	rand_elem(X),
	rand_elem(L).

rand_elem(tuple(X)) :- rand_elem_tuple(X).

rand_elem_tuple([]).
rand_elem_tuple([X|L]) :-
	rand_elem(X),
	rand_elem_tuple(L).


%%%% Write element

write_elem(lit(int,X)) :-  !, write(X).
write_elem(lit(float,X)) :- !,  write(X).
write_elem(lit(atom,X)) :- !,  write(X).

write_elem(nil) :- !, write('[]').
write_elem(cons(X,L)) :- !, write('['), write_elem_(cons(X,L)) ,write(']').

write_elem(tuple(X)) :- !, write_elem_tuple(X).
write_elem(X) :- ground(X), write(X).

write_elem_tuple([]) :- write('{}').
write_elem_tuple([X|L]) :- write('{'), write_elem_tuple_([X|L]) ,write('}').



write_elem_(cons(X,nil)) :- write_elem(X).
write_elem_(cons(X,cons(Y,L))) :-  write_elem(X), write(','), write_elem_(cons(Y,L)).

write_elem_tuple_([X]) :- write_elem(X).
write_elem_tuple_([X,Y|L]) :-  write_elem(X), write(','), write_elem_tuple_([Y|L]).




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
  current_predicate(Prop/Arity),
  functor(CallA,Prop,Arity),
  atom_number(NS,N),
  gtime(
    generate_test_cases(CallA,N) ).

% % Code borrowed from Richard O'Keefe, The Craft of Prolog
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% % repeat a number of times
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
  format(atom(Msec),'~2f~n', [Time/1000]),
  flush_output,
  open('timings.txt',append,Fd,[]),
	write(Fd,Msec),
  close(Fd).

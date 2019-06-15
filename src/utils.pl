%% zip_binds(vars,values,binds)
%% tuples a list of variables with their values
zip_binds([],[],[]).
zip_binds([var(Par)|RPars],[Val|Vals],[(Par,Val)|RPars1]) :-
  zip_binds(RPars,Vals,RPars1).

% has_not_length(list,length)
has_not_length(L,N) :- N==0, L = [_|_].
has_not_length(L,N) :- N>=1, L = [].
has_not_length(L,N) :- N>=1, L = [_|T], M is N-1,
  when(nonvar(T), has_not_length(T,M)).

% diff_len(list,list)
diff_len([],[_|_]).
diff_len([_|_],[]).
diff_len([_|T1],[_|T2]) :-
  when((nonvar(T1),nonvar(T2)), diff_len(T1,T2)).

% same_len(list,list)
same_len([],[]).
same_len([_|T1],[_|T2]) :-
  when((nonvar(T1),nonvar(T2)), same_len(T1,T2)).

% MODE: rsel(-X,+L)
% SEMANTICS: X is a randomly selected member of L.
rsel(X,L) :- random_select(E,L,R), rsel_(X,[E|R]).
% rsel utility predicate
rsel_(X,[X|_]).
rsel_(X,[_|R]) :- rsel(X,R).

% MODE: replace_free_vars(+Term0,+Env,-Term1)
% SEMANTICS: Term1 is obtained from Term0 by replacing all
% occurrences of var(Name) with Val if (Name,Val) belongs to Env
replace_free_vars(Term, _, Term) :-
  var(Term), % Prolog variable
 !.
replace_free_vars(Term0,Env, Term1) :-
  Term0 = var(Name), % Erlang Variable
  memberchk((Name,Val),Env),
  !,
  Term1 = Val.
replace_free_vars(Term0,_, Term1) :-
  Term0 == [],
  !,
  Term1 = Term0.
replace_free_vars([Term|Terms],Env, [Term1|Terms1]) :-
  replace_free_vars(Term,Env, Term1),
  !,
  replace_free_vars(Terms,Env, Terms1).
replace_free_vars(Term0,Env, Term1) :-
  Term0 =..[Funct|Args0],
  replace_free_vars(Args0,Env, Args1),
  Term1 =..[Funct|Args1].
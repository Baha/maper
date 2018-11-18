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

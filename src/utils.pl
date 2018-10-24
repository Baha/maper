%% zip_binds(vars,values,binds)
%% tuples a list of variables with their values
zip_binds([],[],[]).
zip_binds([var(Par)|RPars],[Val|Vals],[(Par,Val)|RPars1]) :-
  zip_binds(RPars,Vals,RPars1).

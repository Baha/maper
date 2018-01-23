:- include('utils').

match(IEnv,IExps,_,IEnv,IExps) :-
  IEnv = (bot,_).
match(IEnv,IExps,Clauses,JEnv,CExp) :-
  IEnv = (top,IBinds),
  match_list(IEnv,IExps,Clauses,CEnv,CExp),
  CEnv = (Error,CBinds),
  append(IBinds,CBinds,JBinds),
  JEnv = (Error,JBinds).

match_list(Env,Exps,[Clause|_],CEnv,CExp) :-
  match_clause(Env,Exps,Clause,CEnv,CExp,true).
match_list(Env,Exps,[Clause|Clauses],CEnv,CExp) :-
  match_clause(Env,Exps,Clause,_,_,false),
  match_list(Env,Exps,Clauses,CEnv,CExp).

match_clause(IEnv,IExps,clause(Pats,_,Body),CEnv,Body,true) :-
  match_pats(IEnv,IExps,Pats,CEnv,true).

  %% CEnv = (top,CBinds),
  %% match_guard(CBinds,Guard,true).
%%
%% TODO: Not assume guards are always true
%% match_guard(_,_,true).
match_clause(_,_,_,_,_,false).

match_pats(Env,[],[],Env,true).
match_pats(Env,[Exp|Exps],[Pat|Pats],FEnv,Res) :-
  match_pat(Env,Exp,Pat,NEnv,true),
  match_pats(NEnv,Exps,Pats,FEnv,Res).
match_pats(Env,[Exp|_],[Pat|_],FEnv,false) :-
  match_pat(Env,Exp,Pat,FEnv,false).


match_pat(Env,lit(X),lit(X),Env,true).
match_pat(Env,Val,var(Var),Env,true) :-
  Env = (_,Binds),
  var_lookup(Var,Binds,Val),
  Val \== undef.
match_pat(Env,Val,var(Var),NEnv,true) :-
  Env = (Error,Binds),
  var_lookup(Var,Binds,undef),
  append(Binds,[(Var,Val)],NBinds),
  NEnv = (Error,NBinds).
match_pat(_,_,_,_,false).

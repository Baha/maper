:- include('utils').

%% match(init_env,init_exps,clauses,match_env,match_exp)
%% matches a list of expressions with a list of clauses and
%% returns the resulting environment and expression (clause body)
match(IEnv,IExps,_,IEnv,IExps) :-
  IEnv = (bot,_).
match(IEnv,IExps,Clauses,CEnv,CExp) :-
  IEnv = (top,_),
  match_list(IEnv,IExps,Clauses,CEnv,CExp).

match_list(Env,Exps,[Clause|_],CEnv,CExp) :-
  match_clause(Env,Exps,Clause,CEnv,CExp,true).
match_list(Env,Exps,[Clause|Clauses],CEnv,CExp) :-
  match_clause(Env,Exps,Clause,_Env,_Exp,false),
  match_list(Env,Exps,Clauses,CEnv,CExp).

%% match_clause(init_env,init_exps,clause,match_env,match_exp,result)
%% matches a list of expressions with a clause and
%% returns the result (and environment and expression if true)
match_clause(IEnv,IExps,clause(Pats,Guard,Body),CEnv,Body,GRes) :-
  match_pats(IEnv,IExps,Pats,CEnv,true),
  match_guard(CEnv,Guard,GRes).
match_clause(IEnv,IExps,clause(Pats,_Guard,_Body1),CEnv,_Body2,false) :-
  match_pats(IEnv,IExps,Pats,CEnv,false).

%% match_pats(init_env,init_exps,patterns,match_env,result)
%% matches a list of expressions with a list of patterns and
%% returns the resulting environment if result is true
match_pats(Env,[],[],Env,true).
match_pats(Env,[Exp|Exps],[Pat|Pats],FEnv,Res) :-
  match_pat(Env,Exp,Pat,NEnv,true),
  match_pats(NEnv,Exps,Pats,FEnv,Res).
match_pats(Env,[Exp|_],[Pat|_],FEnv,false) :-
  match_pat(Env,Exp,Pat,FEnv,false).

%% match_pat(init_env,expression,pattern,match_env,result)
%% matches an expression against a pattern
match_pat(Env,lit(Type,Val),lit(Type,Val),Env,true).
match_pat(Env,tuple(ExpElems),tuple(PatElems),FEnv,true) :-
  match_pats(Env,ExpElems,PatElems,FEnv,true).
match_pat(Env,cons(ExpHead,ExpTail),cons(PatHead,PatTail),FEnv,true) :-
  match_pat(Env,ExpHead,PatHead,MEnv,true),
  match_pat(MEnv,ExpTail,PatTail,FEnv,true).
match_pat(Env,Val,var(Var),FEnv,true) :-
  Env =  (St,BindsIn),
  var_binding(Var,BindsIn,Val,BindsOut),
  FEnv = (St,BindsOut).
match_pat(Env,Exp,Pat,Env,false) :-
  tr(cf(Env,Pat),cf(_NEnv,EPat)),
  \+ subsumes_term(EPat,Exp),
  dif(Exp,EPat).

%% match_guard(env,exp,result)
%% returns true if guard evaluates to true
match_guard(Env,Guard,true) :-
  tr(cf(Env,Guard),cf(_,lit(atom,true))).
match_guard(Env,Guard,false) :-
  tr(cf(Env,Guard),cf(_,lit(atom,false))).

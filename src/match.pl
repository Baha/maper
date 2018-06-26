:- include('utils').

%% match(init_env,init_exps,clauses,match_env,match_exp)
%% matches a list of expressions with a list of clauses and
%% returns the resulting environment and expression (clause body)
match(_B,IEnv,IExps,_,IEnv,IExps) :-
  IEnv = (bot,_).
match(B1,IEnv,IExps,Clauses,CEnv,CExp) :-
  IEnv = (top,_),
  match_list(B1,IEnv,IExps,Clauses,CEnv,CExp).

match_list(B1,Env,Exps,[Clause|_],CEnv,CExp) :-
  match_clause(B1,Env,Exps,Clause,CEnv,CExp,true).
match_list(B1,Env,Exps,[Clause|Clauses],CEnv,CExp) :-
  match_clause(B1,Env,Exps,Clause,_Env,_Exp,false),
  match_list(B1,Env,Exps,Clauses,CEnv,CExp).

%% match_clause(init_env,init_exps,clause,match_env,match_exp,result)
%% matches a list of expressions with a clause and
%% returns the result (and environment and expression if true)
match_clause(B1,IEnv,IExps,clause(Pats,Guard,Body),CEnv,Body,GRes) :-
  match_pats(B1,IEnv,IExps,Pats,CEnv,true),
  match_guard(B1,CEnv,Guard,GRes).
match_clause(B1,IEnv,IExps,clause(Pats,_Guard,_Body1),CEnv,_Body2,false) :-
  match_pats(B1,IEnv,IExps,Pats,CEnv,false).

%% match_pats(init_env,init_exps,patterns,match_env,result)
%% matches a list of expressions with a list of patterns and
%% returns the resulting environment if result is true
match_pats(_B,Env,[],[],Env,true).
match_pats(B1,Env,[Exp|Exps],[Pat|Pats],FEnv,Res) :-
  match_pat(B1,Env,Exp,Pat,NEnv,true),
  match_pats(B1,NEnv,Exps,Pats,FEnv,Res).
match_pats(B1,Env,[Exp|_],[Pat|_],FEnv,false) :-
  match_pat(B1,Env,Exp,Pat,FEnv,false).

%% match_pat(init_env,expression,pattern,match_env,result)
%% matches an expression against a pattern
match_pat(_B,Env,lit(Type1,Val),lit(Type2,Val),Env,true) :-
  subtype(Type1,Type2).
match_pat(B1,Env,tuple(ExpElems),tuple(PatElems),FEnv,true) :-
  match_pats(B1,Env,ExpElems,PatElems,FEnv,true).
match_pat(B1,Env,cons(ExpHead,ExpTail),cons(PatHead,PatTail),FEnv,true) :-
  match_pat(B1,Env,ExpHead,PatHead,MEnv,true),
  match_pat(B1,MEnv,ExpTail,PatTail,FEnv,true).
match_pat(_B,Env,Val,var(Var),FEnv,true) :-
  Env =  (St,BindsIn),
  var_binding(Var,BindsIn,Val,BindsOut),
  FEnv = (St,BindsOut).
match_pat(B1,Env,Exp,Pat,Env,false) :-
  btr(B1,cf(Env,Pat),cf(_NEnv,EPat)),
  %\+ subsumes(EPat,Exp), % SICS
  \+ subsumes_term(EPat,Exp),
  dif(Exp,EPat).

%% match_guard(env,exp,result)
%% returns true if guard evaluates to true
match_guard(B1,Env,Guard,true) :-
  btr(B1,cf(Env,Guard),cf(_,lit(atom,true))).
match_guard(B1,Env,Guard,false) :-
  btr(B1,cf(Env,Guard),cf(_,lit(atom,false))).

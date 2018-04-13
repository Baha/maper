:- include('utils').

%% match(init_env,init_exps,clauses,match_env,match_exp)
%% matches a list of expressions with a list of clauses and
%% returns the resulting environment and expression (clause body)
match(B1,IEnv,IExps,_,B1,IEnv,IExps) :-
  IEnv = (bot,_).
match(B1,IEnv,IExps,Clauses,B2,CEnv,CExp) :-
  IEnv = (top,_),
  match_list(B1,IEnv,IExps,Clauses,B2,CEnv,CExp).

match_list(B1,Env,Exps,[Clause|_],B2,CEnv,CExp) :-
  match_clause(B1,Env,Exps,Clause,B2,CEnv,CExp,true).
match_list(B1,Env,Exps,[Clause|Clauses],B3,CEnv,CExp) :-
  match_clause(B1,Env,Exps,Clause,B2,_Env,_Exp,false),
  match_list(B2,Env,Exps,Clauses,B3,CEnv,CExp).

%% match_clause(init_env,init_exps,clause,match_env,match_exp,result)
%% matches a list of expressions with a clause and
%% returns the result (and environment and expression if true)
match_clause(B1,IEnv,IExps,clause(Pats,Guard,Body),B3,CEnv,Body,GRes) :-
  match_pats(B1,IEnv,IExps,Pats,B2,CEnv,true),
  match_guard(B2,CEnv,Guard,B3,GRes).
match_clause(B1,IEnv,IExps,clause(Pats,_Guard,_Body1),B2,CEnv,_Body2,false) :-
  match_pats(B1,IEnv,IExps,Pats,B2,CEnv,false).

%% match_pats(init_env,init_exps,patterns,match_env,result)
%% matches a list of expressions with a list of patterns and
%% returns the resulting environment if result is true
match_pats(B1,Env,[],[],B1,Env,true).
match_pats(B1,Env,[Exp|Exps],[Pat|Pats],B3,FEnv,Res) :-
  match_pat(B1,Env,Exp,Pat,B2,NEnv,true),
  match_pats(B2,NEnv,Exps,Pats,B3,FEnv,Res).
match_pats(B1,Env,[Exp|_],[Pat|_],B2,FEnv,false) :-
  match_pat(B1,Env,Exp,Pat,B2,FEnv,false).

%% match_pat(init_env,expression,pattern,match_env,result)
%% matches an expression against a pattern
match_pat(B1,Env,lit(Type,Val),lit(Type,Val),B1,Env,true).
match_pat(B1,Env,tuple(ExpElems),tuple(PatElems),B2,FEnv,true) :-
  match_pats(B1,Env,ExpElems,PatElems,B2,FEnv,true).
match_pat(B1,Env,cons(ExpHead,ExpTail),cons(PatHead,PatTail),B3,FEnv,true) :-
  match_pat(B1,Env,ExpHead,PatHead,B2,MEnv,true),
  match_pat(B2,MEnv,ExpTail,PatTail,B3,FEnv,true).
match_pat(B1,Env,Val,var(Var),B1,FEnv,true) :-
  Env =  (St,BindsIn),
  var_binding(Var,BindsIn,Val,BindsOut),
  FEnv = (St,BindsOut).
match_pat(B1,Env,Exp,Pat,B2,Env,false) :-
  btr(B1,cf(Env,Pat),B2,cf(_NEnv,EPat)),
  \+ subsumes_term(EPat,Exp),
  dif(Exp,Pat).

%% match_guard(env,exp,result)
%% returns true if guard evaluates to true
match_guard(B1,Env,Guard,B2,true) :-
  btr(B1,cf(Env,Guard),B2,cf(_,lit(atom,true))).
match_guard(B1,Env,Guard,B2,false) :-
  btr(B1,cf(Env,Guard),B2,cf(_,lit(atom,false))).

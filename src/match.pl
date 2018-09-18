:- include('utils').

%% match(init_env,init_exps,clauses,match_env,match_exp)
%% matches a list of expressions with a list of clauses and
%% returns the resulting environment and expression (clause body)
match(IEnv,Exps,[Clause|Clauses],CEnv,CExp) :-
  match_clause(IEnv,Exps,Clause, MEnv,MExp,Res),
  match_cont(Res,MEnv,MExp,Clauses, CEnv,CExp).
%
match_cont(true,IEnv,IExp,_Clauses, IEnv,IExp).
match_cont(false,IEnv,IExps,Clauses, FEnv,FExp) :-
  match(IEnv,IExps,Clauses, FEnv,FExp).

%% match_clause(init_env,init_exps,clause,match_env,match_exp,result)
%% matches a list of expressions with a clause and
%% returns the result (and environment and expression if true)
match_clause(Env,Exprs,clause(Pats,Guard,Body), MEnv,MExp,Res) :-
  match_pats(Env,Exprs,Pats, MEnv,PRes),
  match_guard(PRes,MEnv,Guard,Exprs,Body, MExp,Res).

% match_guard(Cond,Env,Guard,Exprs,BodyE,MEnv,Result)
% if Cond = 'true', then evaluates the guard and return either
% true and the body of the matching clause or
% false and the list of the input expressions
match_guard(true,Env,Guard,Exprs,BodyE, MExp,GRes) :-
  tr(cf(Env,Guard),cf(_,lit(atom,GRes))),
  match_guard_ifte(GRes,Exprs,BodyE, MExp).
% else if Cond = 'false', then return Exprs and false
match_guard(false,_Env,_Guard,Exprs,_BodyE, Exprs,false).

% guard_ifte(Cond,Expr1,Expr2,Expr)
% selects either Exprs or BodyE according to the value of GRes
match_guard_ifte(true,_Exprs,BodyE, BodyE).
match_guard_ifte(false,Exprs,_BodyE, Exprs).

%% match_pats(init_env,init_exps,patterns,match_env,result)
%% matches a list of expressions with a list of patterns and
%% returns the resulting environment if result is true
match_pats(Env,[],[], Env,true).
match_pats(Env,[Exp|Exps],[Pat|Pats], FEnv,Res) :-
  match_pat(Env,Exp,Pat,NEnv),
  match_pats(NEnv,Exps,Pats, FEnv,Res).
match_pats(Env,[Exp|_Exps],[Pat|_Pats], Env,false) :-
  tr(cf(Env,Pat),cf(_,EPat)),
  \+ subsumes_term(EPat,Exp),
  dif(Exp,EPat).

%% match_pat(init_env,expression,pattern,match_env,result)
%% matches an expression against a pattern
% literal
match_pat(Env,lit(Type,Val),lit(Type,Val),Env).
% variable
match_pat(Env,Val,var(Var),FEnv) :-
  var_binding(Var,Env,Val,FEnv).
% list
match_pat(Env,cons(ExpHead,ExpTail),cons(PatHead,PatTail),FEnv) :-
  match_pat(Env,ExpHead,PatHead,MEnv),
  match_pat(MEnv,ExpTail,PatTail,FEnv).
% tuple
match_pat(Env,tuple([]),tuple([]),Env).
match_pat(Env,tuple([ExpElem|ExpElems]),tuple([PatElem|PatElems]),FEnv) :-
  match_pat(Env,ExpElem,PatElem,MEnv),
  match_pat(MEnv,tuple(ExpElems),tuple(PatElems),FEnv).

:- include('match').

:- use_module(library(lists)).
:- use_module(library(terms)).

:- discontiguous tr/2.

%% run(mod,fun,args,final_env,final_exp)
%% loads mod and evaluates fun (from mod)
run(Mod,(Fun,Arity),Args,FEnv,FExp) :-
  retractall(fundef(_,_,_)),
  consult(Mod),
  eval(Mod,Fun/Arity,Args,FEnv,FExp).

%% eval(mod,fun,args,final_env,final_exp)
%% evaluates fun (from mod) application and
%% returns the final environment and expression
eval(Mod,Fun,Args,FEnv,FExp) :-
  init(Mod,Fun,Args,IEnv,IApp),
  tr(cf(IEnv,IApp),cf(FEnv,FExp)).

%% init(mod,fun,args,env,app)
%% initializes fun (from mod) application
%% with the corresponding environment
init(Mod,Fun/Arity,Args,Env,apply(var(Fun,Arity),Pars)) :-
  fun_lookup(lit(atom,Mod),var(Fun,Arity),fun(Pars,_)),
  zip_binds(Pars,Args,Env).

%% (Error) ---------------------------------------------------------------------
tr(cf(Env,error(Reason)),cf(Env,error(Reason))).

%% Values ----------------------------------------------------------------------
%% (Lit)
tr(cf(Env,lit(Type,Val)),cf(Env,lit(Type,Val))).
%% (Var)
tr(cf(Env,var(Var)),cf(FEnv,Val)) :-
  var_binding(Var,Env,Val,FEnv).
%% (Cons)
tr(cf(IEnv,cons(IExp1,IExp2)),cf(FEnv,cons(FExp1,FExp2))) :-
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  tr(cf(MEnv,IExp2),cf(FEnv,FExp2)).
%% (Tuple)
tr(cf(IEnv,tuple([])),cf(IEnv,tuple([]))).
tr(cf(IEnv,tuple([IExp|IExps])),cf(FEnv,tuple([FExp|FExps]))) :-
  tr(cf(IEnv,IExp),cf(MEnv,FExp)),
  tr(cf(MEnv,tuple(IExps)),cf(FEnv,tuple(FExps))).

%% (Let) -----------------------------------------------------------------------
tr(cf(IEnv,let([var(Var)],Expr1,Expr2)),FCf) :-
  tr(cf(IEnv,Expr1),cf(Env1,EExpr1)),
  let_cont(Env1,Var,EExpr1,Expr2, FCf).
% the evaluation of Expr1 succeeds
let_cont(MEnv,Var,EExpr1,Expr2,cf(FEnv,Expr)) :-
  dif(EExpr1,error(_Reason)),
  tr(cf([(Var,EExpr1)|MEnv],Expr2),cf(FEnv,Expr)).
% the evaluation of Expr1 fails
let_cont(Env,_Vars,EExpr1,_Expr2,cf(Env,EExpr1)) :-
  EExpr1 = error(_Reason).

%% (Case) ----------------------------------------------------------------------
tr(cf(IEnv,case(IExp,Clauses)),cf(FEnv,Exp)) :-
  tr(cf(IEnv,tuple(IExp)),cf(MEnv,tuple(MExps))),
  match(MEnv,MExps,Clauses,NEnv,NExp),
  tr(cf(NEnv,NExp),cf(FEnv,Exp)).

%% (Apply) ---------------------------------------------------------------------
tr(cf(IEnv,apply(FName,IExps)),cf(FEnv,Exp)) :-
  % TODO: Pass module here
  fun_lookup(lit(atom,any),FName,fun(Pars,FunBody)),
  tr(cf(IEnv,tuple(IExps)),cf(FEnv,tuple(FExps))),
  zip_binds(Pars,FExps,AppBinds),
  tr(cf(AppBinds,FunBody),cf(_Env,Exp)).

%% (Call) ----------------------------------------------------------------------
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv,FExps1)) :-
  tr(cf(IEnv,tuple(IExps)),cf(FEnv,tuple(FExps))),
  bif(Atom,Fname,FExps, FExps1).

%% (Primop) --------------------------------------------------------------------
tr(cf(IEnv,primop(lit(atom,match_fail),_)),cf(IEnv,error(match_fail))).

%% (Try) -----------------------------------------------------------------------
tr(cf(IEnv,try(Arg,Vars,Body,EVars,Handler)),cf(FEnv,Exp)) :-
  tr(cf(IEnv,Arg),cf(MEnv,MExp)),
  StdVarsBody = (Vars,Body),
  ErrVarsBody = (EVars,Handler),
  try_vars_body(IEnv,MEnv,MExp,StdVarsBody,ErrVarsBody,FEnv,Exp).

%% try_vars_body(init_env,mid_env,mid_exp,
%%               correct_case,error_case,
%%               final_env,final_exp)
%% auxiliar rule that returns final_env and final_exp
%% of a try-catch block depending on mid_env's error symbol
try_vars_body(IEnv,_MEnv,MExp,_,(_ErrVars,ErrBody),IEnv,ErrBody) :-
  dif(MExp,error(_Reason)).
try_vars_body(_IEnv,MEnv,MExp,(CVars,CBody),_,FEnv,Exp) :-
  MExp = error(_Reason),
  ClauseExp = [clause(CVars,lit(atom,true),CBody)],
  CaseExp = case([MExp],ClauseExp),
  tr(cf(MEnv,CaseExp),cf(FEnv,Exp)).

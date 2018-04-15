:- include('match').

:- use_module(library(lists)).
:- use_module(library(terms)).

:- discontiguous btr/4.

%% bounded_int_lst(ls)
bounded_int_list(L,B) :-
  B >= 0,
  int_list(L,B).
bounded_int_list(L,B) :-
  B > 0, B1 is B - 1,
  bounded_int_list(L,B1).

%% int_lst(iL,len)
%% iL is a list of integers of length len
int_list(lit(list,nil),0).
int_list(cons(Hd,Tl),Len) :-
  Len > 0, Len1 is Len - 1,
  Hd = lit(int,_V),
  int_list(Tl,Len1).

%% bounded_run(mod,fun,bound,args,final_env,final_exp)
%% evaluates fun (from mod) application and
%% returns the final environment and expression
bounded_run(Mod,(Fun,Arity),Bound,Args,FEnv,FExp) :-
  init(Mod,(Fun,Arity),Args,IEnv,IApp),
  btr(Bound,cf(IEnv,IApp),_,cf(FEnv,FExp)).

%% init(mod,fun,args,env,app)
%% initializes fun (from mod) application
%% with the corresponding environment
init(Mod,(Fun,Arity),Args,Env,App) :-
  retractall(fundef(_,_,_)),
  consult(Mod),
  fun_lookup(lit(atom,Mod),var(Fun,Arity),FunDef),
  FunDef = fun(Pars,_),
  zip_binds(Pars,Args,Binds),
  Env = (top,Binds),
  App = apply(var(Fun,Arity),Pars).

%% tr_list(init_env,init_exps,final_env,final_exps)
%% evaluates a list of transitions (from exp to exp) and
%% returns the final environment and expressions
tr_list(B1,IEnv,[],B1,IEnv,[]).
tr_list(B1,IEnv,[IExp|IExps],B3,FEnv,[FExp|FExps]) :-
  btr(B1,cf(IEnv,IExp),B2,cf(NEnv,FExp)),
  tr_list(B2,NEnv,IExps,B3,FEnv,FExps).

%% (Error) ---------------------------------------------------------------------
btr(B1,cf(Env,Exp),B2,cf(Env,Exp)) :-
  Env = (bot,_),
  B1 > 0, B2 is B1 - 1.

%% Values ----------------------------------------------------------------------
%% (Lit)
btr(B1,cf(Env,lit(Type,Val)),B2,cf(Env,lit(Type,Val))) :-
  Env = (top,_),
  B1 > 0, B2 is B1 - 1.

%% (Var)
btr(B1,cf(Env,var(Var)),B2,cf(FEnv,Val)) :-
  Env  = (top,BindsIn),
  B1 > 0, B2 is B1 - 1,
  var_binding(Var,BindsIn,Val,BindsOut),
  FEnv = (top,BindsOut).

%% (Cons)
btr(B1,cf(IEnv,cons(IExp1,IExp2)),B4,cf(FEnv,Exp)) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  btr(B2,cf(IEnv,IExp1),B3,cf(MEnv,FExp1)),
  btr(B3,cf(MEnv,IExp2),B4,cf(FEnv,FExp2)),
  Exp = cons(FExp1,FExp2).

%% (Tuple)
btr(B1,cf(IEnv,tuple(IExps)),B3,cf(FEnv,Exp)) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  tr_list(B2,IEnv,IExps,B3,FEnv,FExps),
  Exp = tuple(FExps).

%% (Let) -----------------------------------------------------------------------
btr(B1,cf(IEnv,let(Vars,IExp1,IExp2)),B4,FCf) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  btr(B2,cf(IEnv,IExp1),B3,cf(MEnv,FExp1)),
  let_cont(B3,MEnv,let(Vars,FExp1,IExp2),B4,FCf).

% the evaluation of IExp1 succeeds
let_cont(B1,MEnv,let(Vars,FExp1,IExp2),B2,cf(FEnv,Exp)) :-
  MEnv = (top,MBinds),
  zip_binds(Vars,[FExp1],ABinds),
  append(MBinds,ABinds,LBinds),
  LEnv = (top,LBinds),
  btr(B1,cf(LEnv,IExp2),B2,cf(FEnv,Exp)).
% the evaluation of IExp1 fails
let_cont(B1,MEnv,let(_Vars,FExp,_IExp),B1,cf(MEnv,FExp)) :-
  MEnv = (bot,_Binds).

%% (Case) ----------------------------------------------------------------------
btr(B1,cf(IEnv,case(IExp,Clauses)),B5,cf(FEnv,Exp)) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  format_values(IExp,VExps),
  tr_list(B2,IEnv,VExps,B3,MEnv,MExps),
  match(B3,MEnv,MExps,Clauses,B4,NEnv,NExp),
  btr(B4,cf(NEnv,NExp),B5,cf(FEnv,Exp)).

%% (Apply) ---------------------------------------------------------------------
btr(B1,cf(IEnv,apply(FName,IExps)),B4,cf(FEnv3,Exp)) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  % TODO: Pass module here
  fun_lookup(lit(atom,any),FName,FunDef),
  FunDef = fun(Pars,FunBody),
  tr_list(B2,IEnv,IExps,B3,FEnv,FExps),
  zip_binds(Pars,FExps,AppBinds),
  FEnv = (Error,Binds),
  btr(B3,cf((Error,AppBinds),FunBody),B4,cf((Error2,_),Exp)),
  FEnv3 = (Error2,Binds).

%% (Call) ----------------------------------------------------------------------
btr(B1,cf(IEnv,call(Atom,Fname,IExps)),B3,FCf) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  tr_list(B2,IEnv,IExps,B3,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  call_cont(Atom,Fname,CTypes,FEnv,FExps,ETypes,FCf).

% (Call1 - arithmetic error)
call_cont(_Atom,_Fname,CTypes,FEnv1,_FExps,ETypes,cf(FEnv2,error(badarith))) :-
  dif(CTypes,ETypes),
  FEnv1 = (_St,Binds),
  FEnv2 = (bot,Binds).
% (Call2 - execute bif)
call_cont(Atom,Fname,CTypes,FEnv,FExps,ETypes,cf(FEnv,Exp)) :-
  CTypes = ETypes,
  call_cont_bif(Atom,Fname,FExps,Exp).

% (Call2.1 - bif terminates erroneously)
call_cont_bif(Atom,Fname,FExps,error(bad_arg)) :-
  bif(Atom,Fname,FExps,bad_arg).
% (Call2.2 - bif terminates correctly)
call_cont_bif(Atom,Fname,FExps,Exp) :-
  bif(Atom,Fname,FExps,Exp).

%% (Primop) --------------------------------------------------------------------
btr(B1,cf(IEnv,primop(lit(atom,match_fail),_)),B2,cf(FEnv,error(match_fail))) :-
  IEnv = (top,Binds),
  B1 > 0, B2 is B1 - 1,
  FEnv = (bot,Binds).

%% (Try) -----------------------------------------------------------------------
btr(B1,cf(IEnv,try(Arg,Vars,Body,EVars,Handler)),B4,cf(FEnv,Exp)) :-
  IEnv = (top,_),
  B1 > 0, B2 is B1 - 1,
  btr(B2,cf(IEnv,Arg),B3,cf(MEnv,MExp)),
  StdVarsBody = (Vars,Body),
  ErrVarsBody = (EVars,Handler),
  try_vars_body(B3,IEnv,MEnv,MExp,StdVarsBody,ErrVarsBody,B4,FEnv,Exp).

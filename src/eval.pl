:- include('match').

:- discontiguous tr/2.

%% init(mod,fun,args,env,app)
%% initializes fun (from mod) application
%% with the corresponding environment
init(Mod,Fun,Args,Env,App) :-
  length(Args,NArgs),
  fun_lookup(lit(atom,Mod),var(Fun,NArgs),FunDef),
  FunDef = fun(Pars,_),
  zip_binds(Pars,Args,Binds),
  Env = (top,Binds),
  App = apply(var(Fun,NArgs),Pars).

%% run(mod,fun,args,final_env,final_exp)
%% evaluates fun (from mod) application and
%% returns the final environment and expression
run(Mod,(Fun,Arity),Args,FEnv,FExp) :-
  length(Args,NArgs),
  Arity == NArgs,
  retractall(fundef(_,_,_)),
  consult(Mod),
  init(Mod,Fun,Args,IEnv,IApp),
  tr(cf(IEnv,IApp),cf(FEnv,FExp)).

run(_,(_,Arity),Args,[],[]) :-
  length(Args,NArgs),
  Arity \== NArgs.

%% tr_list(init_env,init_exps,final_env,final_exps)
%% evaluates a list of transitions (from exp to exp) and
%% returns the final environment and expressions
tr_list(IEnv,[],IEnv,[]).
tr_list(IEnv,[IExp|IExps],FEnv,[FExp|FExps]) :-
  tr(cf(IEnv,IExp),cf(NEnv,FExp)),
  tr_list(NEnv,IExps,FEnv,FExps).

%% (Error)
tr(cf(Env,Exp),cf(Env,Exp)) :-
  Env = (bot,_).

%% (Lit)
tr(cf(Env,lit(Type,Val)),cf(Env,lit(Type,Val))) :-
  Env = (top,_).

%% (Var)
tr(cf(Env,var(Var)),cf(FEnv,Val)) :-
  Env  = (top,BindsIn),
  var_binding(Var,BindsIn,Val,BindsOut),
  FEnv = (top,BindsOut).

%% (Cons)
tr(cf(IEnv,cons(IExp1,IExp2)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  tr(cf(MEnv,IExp2),cf(FEnv,FExp2)),
  Exp = cons(FExp1,FExp2).

%% (Tuple)
tr(cf(IEnv,tuple(IExps)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  Exp = tuple(FExps).

%% TODO: Review this. Can it be done the same way
%% as in the apply rule?
%% (Let)
/*
tr(cf(IEnv,let(Vars,IExp1,IExp2)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  MEnv = (Error,MBinds),
  zip_binds(Vars,[FExp1],ABinds),
  append(MBinds,ABinds,LBinds),
  LEnv = (Error,LBinds),
  tr(cf(LEnv,IExp2),cf(FEnv,FExp2)),
  ite(FEnv,FExp2,Exp).
*/
% =====
tr(cf(IEnv,let(Vars,IExp1,IExp2)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  let_cont(cf(MEnv,FExp1),Vars,IExp2,cf(FEnv,Exp)).
%
let_cont(cf(MEnv,FExp1),Vars,IExp2,cf(FEnv,Exp)) :-
  MEnv = (top,MBinds),
  zip_binds(Vars,[FExp1],ABinds),
  append(MBinds,ABinds,LBinds),
  LEnv = (top,LBinds),
  tr(cf(LEnv,IExp2),cf(FEnv,Exp)).
%
let_cont(cf(MEnv,Exp),_Vars,_IExp2,cf(MEnv,Exp)) :-
  MEnv = (bot,_Binds).

%% (Case)
tr(cf(IEnv,case(IExp,Clauses)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  format_values(IExp,VExps),
  tr_list(IEnv,VExps,MEnv,MExps),
  match(MEnv,MExps,Clauses,NEnv,NExp),
  tr(cf(NEnv,NExp),cf(FEnv,Exp)).

%% (Apply)
tr(cf(IEnv,apply(FName,IExps)),cf(FEnv3,Exp)) :-
  IEnv = (top,_),
  % TODO: Pass module here
  fun_lookup(lit(atom,any),FName,FunDef),
  FunDef = fun(Pars,FunBody),
  tr_list(IEnv,IExps,FEnv,FExps),
  zip_binds(Pars,FExps,AppBinds),
  FEnv = (Error,Binds),
  tr(cf((Error,AppBinds),FunBody),cf((Error2,_),Exp)),
  FEnv3 = (Error2,Binds).

%% (Call1)
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv2,error(badarith))) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  dif(CTypes,ETypes),
  FEnv = (_,Binds),
  FEnv2 = (bot,Binds).

%% (Call2)
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv,error(bad_arg))) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  CTypes = ETypes,
  bif(Atom,Fname,FExps,bad_arg).

%% (Call3)
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  CTypes = ETypes,
  bif(Atom,Fname,FExps,Exp).

%% (Primop)
tr(cf(IEnv,primop(lit(atom,match_fail),_)),cf(FEnv,error(match_fail))) :-
  IEnv = (top,Binds),
  FEnv = (bot,Binds).

%% (Try)
tr(cf(IEnv,try(Arg,Vars,Body,EVars,Handler)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,Arg),cf(MEnv,MExp)),
  StdVarsBody = (Vars,Body),
  ErrVarsBody = (EVars,Handler),
  try_vars_body(IEnv,MEnv,MExp,StdVarsBody,ErrVarsBody,FEnv,Exp).

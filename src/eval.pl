:- include('match').

init(Mod,Fun,Args,Env,App) :-
  length(Args,NArgs),
  fun_lookup(lit(atom(Mod)),var(Fun,NArgs),FunDef),
  FunDef = fun(Pars,_),
  zip_binds(Pars,Args,Binds),
  Env = (top,Binds),
  App = apply(var(Fun,NArgs),Pars).

%% run(mod,fun,args,final_env,final_exp)
%% evaluates fun (from mod) application and
%% returns the final environment and expression
run(Mod,Fun,Args,FEnv,FExp) :-
  consult(tmp),
  init(Mod,Fun,Args,IEnv,IApp),
  tr(cf(IEnv,IApp),cf(FEnv,FExp)).

%% (Error)
tr(cf(Env,Exp),cf(Env,Exp)) :-
  Env = (bot,_).

%% (Lit)
tr(cf(Env,lit(Lit)),cf(Env,lit(Lit))) :-
  Env = (top,_).

%% (Var)
tr(cf(Env,var(Var)),cf(Env,Val)) :-
  Env = (top,Binds),
  var_lookup(Var,Binds,Val).

%% (Cons)
tr(cf(IEnv,cons(IExp1,IExp2)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  tr(cf(MEnv,IExp2),cf(FEnv,FExp2)),
  ite(FEnv,cons(FExp1,FExp2),Exp).

%% (Tuple)
tr(cf(IEnv,tuple(IExps)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  ite(FEnv,tuple(FExps),Exp).

%% TODO: Review this. Can it be done the same way
%% as in the apply rule?
%% (Let)
tr(cf(IEnv,let(Vars,IExp1,IExp2)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  MEnv = (Error,MBinds),
  zip_binds(Vars,[FExp1],ABinds),
  append(MBinds,ABinds,LBinds),
  LEnv = (Error,LBinds),
  tr(cf(LEnv,IExp2),cf(FEnv,FExp2)),
  ite(FEnv,FExp2,Exp).

%% (Case)
tr(cf(IEnv,case(IExp,Clauses)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  format_values(IExp,VExps),
  tr_list(IEnv,VExps,MEnv,MExps),
  match(MEnv,MExps,Clauses,NEnv,NExp),
  tr(cf(NEnv,NExp),cf(FEnv,FExp)),
  ite(FEnv,FExp,Exp).

%% (Apply)
tr(cf(IEnv,apply(FName,IExps)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  % TODO: Pass module here
  fun_lookup(lit(atom(any)),FName,FunDef),
  FunDef = fun(Pars,FunBody),
  tr_list(IEnv,IExps,FEnv,FExps),
  zip_binds(Pars,FExps,AppBinds),
  FEnv = (Error,_),
  tr(cf((Error,AppBinds),FunBody),cf(FEnv2,FExp)),
  ite(FEnv2,FExp,Exp).

%% (Call1)
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv2,error(badarith))) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  CTypes \== ETypes,
  FEnv = (_,Binds),
  FEnv2 = (bot,Binds).

%% (Call2)
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv,error(bad_arg))) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  CTypes == ETypes,
  bif(Atom,Fname,FExps,bad_arg).

%% (Call3)
tr(cf(IEnv,call(Atom,Fname,IExps)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr_list(IEnv,IExps,FEnv,FExps),
  types(Atom,Fname,CTypes),
  types(FExps,ETypes),
  CTypes == ETypes,
  bif(Atom,Fname,FExps,CRes),
  ite(FEnv,CRes,Exp).

%% (Primop)
tr(cf(IEnv,primop(lit(atom(match_fail)),X)),cf(FEnv,X)) :-
  IEnv = (_,Binds),
  FEnv = (bot,Binds).

tr_list(IEnv,[],IEnv,[]).
tr_list(IEnv,[IExp|IExps],FEnv,[FExp|FExps]) :-
  tr(cf(IEnv,IExp),cf(NEnv,FExp)),
  tr_list(NEnv,IExps,FEnv,FExps).

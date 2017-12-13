:- include('match').

init(Mod,Fun,Args,Env,App) :-
  % obtain arity from Args
  % Add smart init? i.e., checks num args
  length(Args,NArgs),
  fun_lookup(lit(atom(Mod)),var(Fun,NArgs),FunDef),
  FunDef = fun(Pars,_),
  init_binds(Pars,Binds),
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

%% (Let)
tr(cf(IEnv,let(Vars,IExp1,IExp2)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  tr(cf(IEnv,IExp1),cf(MEnv,FExp1)),
  MEnv = (Error,MBinds),
  append(MBinds,(Vars,FExp1),LBinds),
  LEnv = (Error,LBinds),
  tr(cf(LEnv,IExp2),cf(FEnv,FExp2)),
  ite(FEnv,FExp2,Exp).

%% (Case)
tr(cf(IEnv,case(IExp,Clauses)),cf(FEnv,Exp)) :-
  IEnv = (top,_),
  format_values(IExp,VExps),
  tr_list(IEnv,VExps,MEnv,MExps),
  % TODO: Implement match rule
  match(MEnv,MExps,Clauses,NEnv,NExp),
  tr(cf(NEnv,NExp),cf(FEnv,FExp)),
  ite(FEnv,FExp,Exp).

%% (Apply)
tr(cf(IEnv,apply(FName,IExps)),cf(FEnv2,Exp)) :-
  IEnv = (top,_),
  % TODO: Pass module here
  fun_lookup(lit(atom(any)),FName,FunDef),
  FunDef = fun(Pars,FunBody),
  tr_list(IEnv,IExps,FEnv,FExps),
  zip_binds(Pars,FExps,AppBinds),
  FEnv = (Error,FBinds),
  append(FBinds,AppBinds,FullBinds),
  tr(cf((Error,FullBinds),FunBody),cf(FEnv2,FExp)),
  ite(FEnv2,FExp,Exp).

tr_list(IEnv,[],IEnv,[]).
tr_list(IEnv,[IExp|IExps],FEnv,[FExp|FExps]) :-
  tr(cf(IEnv,IExp),cf(NEnv,FExp)),
  tr_list(NEnv,IExps,FEnv,FExps).

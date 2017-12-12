
fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).
  % change pred to moddef

init_binds([],[]).
init_binds([Par|RPars],[(Par,_)|RPars1]) :-
  init_binds(RPars,RPars1).

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

zip_binds([],[],[]).
zip_binds([Var|Vars],[Val|Vals],[(Var,Val)|RBinds]) :-
  zip_binds(Vars,Vals,RBinds).

%% ite(env,exp_in,exp_out)
%% outputs error if env contains bottom, and exp_in otherwise
ite(Env, _, error) :-
  Env = (bot,_).
ite(Env,Exp,Exp) :-
  Env = (top,_).


format_values(values(Exps),Exps).
format_values(Exp,[Exp]).

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

match_pats(Env,[],[],Env,true).
match_pats(Env,[Exp|Exps],[Pat|Pats],FEnv,Res) :-
  match_pat(Env,Exp,Pat,NEnv,true),
  match_pats(NEnv,Exps,Pats,FEnv,Res).


match_pat(Env,lit(X),lit(X),Env,true).
%% match_pat(Env,Val,var(Var),Env,true) :-
%%   var_lookup(Var,Env)
match_pat(Env,_,_,Env,false).

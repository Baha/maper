
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


run(Mod,Fun,Args,Env2,Exp) :-
  consult(tmp),
  init(Mod,Fun,Args,Env,App),
  tr(cf(Env,App),cf(Env2,Exp)).

%% (Error)
tr(cf(Env,Exp),cf(Env,Exp)) :-
  Env = (bot,_Binds).

%% (Lit)
tr(cf(Env,lit(Lit)),cf(Env,lit(Lit))) :-
  Env = (top,_Binds).

%% (Var)
tr(cf(Env,var(Var)),cf(Env,Val)) :-
  Env = (top,Binds),
  var_lookup(Var,Binds,Val).

%% (Cons)
tr(cf(Env,cons(Exp1,Exp2)),cf(Env2,Exp)) :-
  Env = (top,_),
  tr(cf(Env,Exp1),cf(Env1,RExp1)),
  tr(cf(Env1,Exp2),cf(Env2,RExp2)),
  ite(Env2,cons(RExp1,RExp2),Exp).

%% (Apply)
tr(cf(Env,apply(Fname,Exprs)),cf(LEnv2,Exp)) :-
  Env = (top,_),
  % TODO: Pass module here
  fun_lookup(lit(atom(any)),Fname,FunDef),
  FunDef = fun(Pars,Body),
  tr_list(Env,Exprs,LEnv,LExprs),
  zip_binds(Pars,LExprs,Zips),
  LEnv = (Error,LBinds),
  append(LBinds,Zips,NewBinds),
  tr(cf((Error,NewBinds),Body),cf(LEnv2,Body2)),
  ite(LEnv2,Body2,Exp).

tr_list(Env1,[],Env1,[]).
tr_list(Env1,[Exp|Exps],Env3,[Exp2|Exps2]) :-
  tr(cf(Env1,Exp),cf(Env2,Exp2)),
  tr_list(Env2,Exps,Env3,Exps2).

zip_binds([],[],[]).
zip_binds([X|Xs],[Y|Ys],[(X,Y)|Rest]) :-
  zip_binds(Xs,Ys,Rest).

%% ite(env,exp_in,exp_out)
%% outputs error if env contains bottom, and exp_in otherwise
ite(Env, _, error) :-
  Env = (bot,_).
ite(Env,Exp1,Exp1) :-
  Env = (top,_).

%% consult(tmp.pl)
%% add facts to some file
%% then consult that file and match in lookup

lookup(Mod,Fun,FunDef) :-
  fundef(Mod,Fun,FunDef).
  % change pred to moddef

init_binds([],[]).
init_binds([Par|RPars],[(Par,_)|RPars1]) :-
  init_binds(RPars,RPars1).

init(Mod,Fun,Args,Env,App) :-
  % obtain arity from Args
  % Add smart init? i.e., checks num args
  length(Args,N),
  fun_lookup(Mod,var(Fun,N),FunDef),
  FunDef = fun(Pars,_),
  init_binds(Pars,Binds),
  Env = (top,Binds),
  App = apply(Fun,Pars).

run(Fun,Args,Env2,Exp) :-
  init(Fun,Args,Env,App),
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

%% ite(env,exp_in,exp_out)
%% outputs error if env contains bottom, and exp_in otherwise
ite(Env, _, error) :-
  Env = (bot,_).
ite(Env,Exp1,Exp1) :-
  Env = (top,_).

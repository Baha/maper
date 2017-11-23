:- use_module(utils).

%% (Error)
tr(cf(Env,Exp),cf(Env,Exp)) :- member(bot,Env).

%% (Lit)
tr(cf(Env,lit(Lit)),cf(Env,lit(Lit))) :- \+ member(bot,Env).

%% (Var)
tr(cf(Env,var(Var)),cf(Env,Val)) :-
  \+ member(bot,Env),
  lookup(Var,Env,Val).

tr(cf(Env,cons(Exp1,Exp2)),cf(Env2,Exp)) :-
  \+ member(bot,Env),
  tr(cf(Env,Exp1),cf(Env1,RExp1)),
  tr(cf(Env1,Exp2),cf(Env2,RExp2)),
%  ite(Env2,)

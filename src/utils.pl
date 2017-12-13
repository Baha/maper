fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).

init_binds([],[]).
init_binds([Par|RPars],[(Par,_)|RPars1]) :-
  init_binds(RPars,RPars1).

format_values(values(Exps),Exps).
format_values(Exp,[Exp]).

%% ite(env,exp_in,exp_out)
%% outputs error if env contains bottom, and exp_in otherwise
ite(Env, _, error) :-
  Env = (bot,_).
ite(Env,Exp,Exp) :-
  Env = (top,_).

zip_binds([],[],[]).
zip_binds([Var|Vars],[Val|Vals],[(Var,Val)|RBinds]) :-
  zip_binds(Vars,Vals,RBinds).
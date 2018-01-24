fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).

var_lookup(_,[],undef).
var_lookup(Var,[(Var,Val)|_],Val).
var_lookup(Var,[(Var1,_)|REnv],Val) :-
  Var \== Var1,
  var_lookup(Var,REnv,Val).

zip_binds([],[],[]).
zip_binds([var(Par)|RPars],[Val|Vals],[(Par,Val)|RPars1]) :-
  zip_binds(RPars,Vals,RPars1).

format_values(values(Exps),Exps).
format_values(Exp,[Exp]).

%% ite(env,exp_in,exp_out)
%% outputs error if env contains bottom, and exp_in otherwise
ite(Env, _, error) :-
  Env = (bot,_).
ite(Env,Exp,Exp) :-
  Env = (top,_).

type(lit(int(_)),int).
type(lit(atom(_)),atom).

types([],[]).
types([Term|Terms],[Type|Types]) :-
  type(Term,Type),
  types(Terms,Types).

types(lit(atom(erlang)),lit(atom('+')),[int,int]).
types(lit(atom(erlang)),lit(atom('-')),[int,int]).
types(lit(atom(erlang)),lit(atom('*')),[int,int]).
types(lit(atom(erlang)),lit(atom('/')),[int,int]).

bif(lit(atom(erlang)),lit(atom('+')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is X+Y.
bif(lit(atom(erlang)),lit(atom('-')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is X-Y.
bif(lit(atom(erlang)),lit(atom('*')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is X*Y.
bif(lit(atom(erlang)),lit(atom('/')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is div(X,Y).

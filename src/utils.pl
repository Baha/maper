:- use_module(library(clpq)).

fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).

%% var_binding(var,env,result,env1)
%% searches a variable var in environment env and returns its value val
%% if var belongs to env, otherwise it adds the pair (var,val) to env.
var_binding(Var,[],Val,[Bind]) :-
  !,
  Bind = (Var,Val).
var_binding(Var,[Bind|REnv1],Val,[Bind|REnv1]) :-
  Bind = (Var1,Val1), Var == Var1,
  !,
  Val = Val1.
var_binding(Var,[Bind|REnv1],Val,[Bind|Renv2]) :-
  var_binding(Var,REnv1,Val,Renv2).

%% zip_binds(vars,values,binds)
%% tuples a list of variables with their values
zip_binds([],[],[]).
zip_binds([var(Par)|RPars],[Val|Vals],[(Par,Val)|RPars1]) :-
  zip_binds(RPars,Vals,RPars1).

%% format_values(exp_in,exp_out)
%% puts exp_in in a list if it is a single expression
format_values(values(Exps),Exps).
format_values(Exp,[Exp]).

%% ite(env,exp_in,exp_out)
%% outputs error if env contains bottom, and exp_in otherwise
ite(Env, Exp, Exp) :-
  Env = (bot,_).
ite(Env,Exp,Exp) :-
  Env = (top,_).

%% try_vars_body(init_env,mid_env,mid_exp,
%%               correct_case,error_case,
%%               final_env,final_exp)
%% auxiliar rule that returns final_env and final_exp
%% of a try-catch block depending on mid_env's error symbol
try_vars_body(IEnv,MEnv,_,_,(_ErrVars,ErrBody),IEnv,ErrBody) :-
  MEnv = (bot,_).
try_vars_body(_IEnv,MEnv,MExp,(CVars,CBody),_,FEnv,Exp) :-
  MEnv = (top,_),
  ClauseExp = [clause(CVars,lit(atom,true),CBody)],
  CaseExp = case(MExp,ClauseExp),
  tr(cf(MEnv,CaseExp),cf(FEnv,Exp)).

%% type(term,type)
%% returns the type of term
type(lit(Type,_),Type).

%% types(terms,type)
%% returns the types of a list of terms
types([],[]).
types([Term|Terms],[Type|Types]) :-
  type(Term,Type),
  types(Terms,Types).

%% types(mod,fun,types)
%% returns the expected types for a given BIF
types(lit(atom,erlang),lit(atom,'+'),[int,int]).
types(lit(atom,erlang),lit(atom,'-'),[int,int]).
types(lit(atom,erlang),lit(atom,'*'),[int,int]).
types(lit(atom,erlang),lit(atom,'/'),[int,int]).
types(lit(atom,erlang),lit(atom,'=='),[int,int]).
types(lit(atom,erlang),lit(atom,'/='),[int,int]).
types(lit(atom,erlang),lit(atom,'=<'),[int,int]).
types(lit(atom,erlang),lit(atom,'<'),[int,int]).
types(lit(atom,erlang),lit(atom,'>='),[int,int]).
types(lit(atom,erlang),lit(atom,'>'),[int,int]).
types(lit(atom,erlang),lit(atom,'=:='),[int,int]).
types(lit(atom,erlang),lit(atom,'=/='),[int,int]).
types(lit(atom,erlang),lit(atom,'=='),[atom,atom]).
types(lit(atom,erlang),lit(atom,'/='),[atom,atom]).
types(lit(atom,erlang),lit(atom,'=:='),[atom,atom]).
types(lit(atom,erlang),lit(atom,'=/='),[atom,atom]).

%% bif(mod,fun,inputs,outputs)
%% emulates the execution of a given BIF

%% atom comparisons
bif(lit(atom,erlang),lit(atom,'=='),[X,X], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'=='),[X,Y], lit(atom,false)) :-
  dif(X,Y).
bif(lit(atom,erlang),lit(atom,'/='),[X,X], lit(atom,false)).
bif(lit(atom,erlang),lit(atom,'/='),[X,Y], lit(atom,true)) :-
  dif(X,Y).
%% '=:=' and '=/=' are exact comparisons (i.e., 2.0 =:= 2 is false),
%% but we implement them as '==' and '/=' since we only consider integers
bif(lit(atom,erlang),lit(atom,'=:='),[X,X], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'=:='),[X,Y], lit(atom,false)) :-
  dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=/='),[X,X], lit(atom,false)).
bif(lit(atom,erlang),lit(atom,'=/='),[X,Y], lit(atom,true)) :-
  dif(X,Y).

%% integer comparisons
bif(lit(atom,erlang),lit(atom,'=<'),[lit(int,X),lit(int,Y)], lit(atom,true)) :-
  {X =< Y}.
bif(lit(atom,erlang),lit(atom,'=<'),[lit(int,X),lit(int,Y)], lit(atom,false)) :-
  {X > Y}.
bif(lit(atom,erlang),lit(atom,'<'),[lit(int,X),lit(int,Y)], lit(atom,true)) :-
  {X < Y}.
bif(lit(atom,erlang),lit(atom,'<'),[lit(int,X),lit(int,Y)], lit(atom,false)) :-
  {X >= Y}.
bif(lit(atom,erlang),lit(atom,'>='),[lit(int,X),lit(int,Y)], lit(atom,true)) :-
  {X >= Y}.
bif(lit(atom,erlang),lit(atom,'>='),[lit(int,X),lit(int,Y)], lit(atom,false)) :-
  {X < Y}.
bif(lit(atom,erlang),lit(atom,'>'),[lit(int,X),lit(int,Y)], lit(atom,true)) :-
  {X > Y}.
bif(lit(atom,erlang),lit(atom,'>'),[lit(int,X),lit(int,Y)], lit(atom,false)) :-
  {X =< Y}.

%% integer arithmetic operations
bif(lit(atom,erlang),lit(atom,'+'),[lit(int,X),lit(int,Y)], lit(int,Z)) :-
  {Z = X + Y}.
bif(lit(atom,erlang),lit(atom,'-'),[lit(int,X),lit(int,Y)], lit(int,Z)) :-
  {Z = X - Y}.
bif(lit(atom,erlang),lit(atom,'*'),[lit(int,X),lit(int,Y)], lit(int,Z)) :-
  {Z = X * Y}.
bif(lit(atom,erlang),lit(atom,'/'),[lit(int,X),lit(int,Y)], lit(int,Z)) :-
  {Z = X / Y}.

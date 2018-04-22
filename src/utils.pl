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

%% try_vars_body(init_env,mid_env,mid_exp,
%%               correct_case,error_case,
%%               final_env,final_exp)
%% auxiliar rule that returns final_env and final_exp
%% of a try-catch block depending on mid_env's error symbol
try_vars_body(_B,IEnv,MEnv,_,_,(_ErrVars,ErrBody),IEnv,ErrBody) :-
  MEnv = (bot,_).
try_vars_body(B1,_IEnv,MEnv,MExp,(CVars,CBody),_,FEnv,Exp) :-
  MEnv = (top,_),
  ClauseExp = [clause(CVars,lit(atom,true),CBody)],
  CaseExp = case(MExp,ClauseExp),
  btr(B1,cf(MEnv,CaseExp),cf(FEnv,Exp)).

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
types(lit(atom,erlang),lit(atom,Op),  [number,number]) :-
  memberchk(Op,['+','-','*','/','==','/=','=<','<','>=','>','=:=','=/=']).
types(lit(atom,erlang),lit(atom,'=='),[atom,atom]).
types(lit(atom,erlang),lit(atom,'/='),[atom,atom]).
types(lit(atom,erlang),lit(atom,'=:='),[atom,atom]).
types(lit(atom,erlang),lit(atom,'=/='),[atom,atom]).

:- discontiguous bif/4.
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
bif(lit(atom,erlang),lit(atom,'=:='),[X,X], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'=:='),[X,Y], lit(atom,false)) :-
  dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=/='),[X,X], lit(atom,false)).
bif(lit(atom,erlang),lit(atom,'=/='),[X,Y], lit(atom,true)) :-
  dif(X,Y).

%% number comparisons
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,X),lit(T2,Y)], lit(atom,Res)) :-
  subtype(T1,number) , subtype(T2,number), bif_rel_op(Op,X,Y,Res).

%% relational operators
bif_rel_op('=<',X,Y,true) :- {X =< Y}.
bif_rel_op('<', X,Y,true) :- {X < Y}.
bif_rel_op('>=',X,Y,true) :- {X >= Y}.
bif_rel_op('>', X,Y,true) :- {X > Y}.
bif_rel_op('=<',X,Y,false) :- {X > Y}.
bif_rel_op('<', X,Y,false) :- {X >= Y}.
bif_rel_op('>=',X,Y,false) :- {X < Y}.
bif_rel_op('>', X,Y,false) :- {X =< Y}.

%% arithmetic operations
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,X),lit(T2,Y)], lit(T3,Z)) :-
  subtype(T1,number) , subtype(T2,number), numerical_literal_res(T1,T2,T3),
  bif_arith_op(Op,X,Y,Z).

%% arithmetic operators
bif_arith_op('+',X,Y,Z) :- {Z = X + Y}.
bif_arith_op('-',X,Y,Z) :- {Z = X - Y}.
bif_arith_op('*',X,Y,Z) :- {Z = X * Y}.
bif_arith_op('/',X,Y,Z) :- {Z = X / Y}.

%% subtypes(types1,types2)
%% types1 and types2 are two lists of types of the same length and
%% forall i, types1[i] is a subtype of types2[i]
subtypes([],[]).
subtypes([T1|T1s],[T2|T2s]) :-
  subtype(T1,T2),
  subtypes(T1s,T2s).

%% subtype(A,B)
%% A is a subtype of B
subtype(T1,T2) :- ground(T1), T1 == float, T2 == number.
subtype(T1,T2) :- ground(T1), T1 == int,   T2 == number.
subtype(T1,T2) :- T1 = T2.

%% diftypes(types1,types2)
%% types1 and types2 are two lists of types that differ in at least one element
diftypes([T1|T1s],[T2|T2s]) :-
  subtype(T1,T2),
  diftypes(T1s,T2s).
diftypes([T1|_T1s],[T2|_T2s]) :-
  var(T1),
  dif(T1,T2).

%% assumption: T1 and T2 are ground terms (due to the calls to subtype)
numerical_literal_res(T1,T2,T3) :-
  T1 == T2,
  T3 = T1.
numerical_literal_res(T1,T2,T3) :-
  T1 \== T2,
  T1 == float,
  T3 = float.
numerical_literal_res(T1,T2,T3) :-
  T1 \== T2,
  T2 == float,
  T3 = float.
numerical_literal_res(T1,T2,T3) :-
  T1 \== T2,
  T1 == number,
  T3 = number.
numerical_literal_res(T1,T2,T3) :-
  T1 \== T2,
  T2 == number,
  T3 = number.

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
types(lit(atom,erlang),lit(atom,Op), [T1,T2]) :-
  rop(Op),
  member(T1,[number,atom]), member(T2,[number,atom]).
types(lit(atom,erlang),lit(atom,Op), [T1,T2]) :-
  aop(Op),
  T1 = number, T2 = number.

% ASSUMPTION: aop and rop are always called with Op ground
% arithmetic operators
aop(Op) :- memberchk(Op,['+','-','*','/']).
% relational operators
rop(Op) :- memberchk(Op,['==','/=','=<','<','>=','>','=:=','=/=']).

:- discontiguous bif/4.
%% bif(mod,fun,inputs,outputs)
%% emulates the execution of a given BIF

%% atoms comparisons
bif(lit(atom,erlang),lit(atom,'=='),[X,X], lit(atom,true) ).
bif(lit(atom,erlang),lit(atom,'=='),[X,Y], lit(atom,false)) :- dif(X,Y).
bif(lit(atom,erlang),lit(atom,'/='),[X,Y], lit(atom,true) ) :- dif(X,Y).
bif(lit(atom,erlang),lit(atom,'/='),[X,X], lit(atom,false)).
%% '=:=' and '=/=' are exact comparisons (i.e., 2.0 =:= 2 is false)
bif(lit(atom,erlang),lit(atom,'=:='),[X,X], lit(atom,true) ).
bif(lit(atom,erlang),lit(atom,'=:='),[X,Y], lit(atom,false)) :- dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=/='),[X,Y], lit(atom,true) ) :- dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=/='),[X,X], lit(atom,false)).
%% numbers comparisons
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,X),lit(T2,Y)], lit(atom,Res)) :-
  rop(Op), subtype(T1,number), subtype(T2,number), bif_rop(Op,X,Y,Res).
%% number-atom comparisons
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,true)) :-
  memberchk(Op,['=<','<','=/=','/=']),
  subtype(T1,number), subtype(T2,atom).
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,false)) :-
  memberchk(Op,['>=','>','==','=:=']),
  subtype(T1,number), subtype(T2,atom).
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,false)) :-
  memberchk(Op,['=<','<','==','=:=']),
  subtype(T1,atom), subtype(T2,number).
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,true)) :-
  memberchk(Op,['>=','>','=/=','/=']),
  subtype(T1,atom), subtype(T2,number).
%% arithmetic operations
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,X),lit(T2,Y)], lit(T3,Z)) :-
  aop(Op), subtype(T1,number) , subtype(T2,number),
  numerical_literal_res(T1,T2,T3),
  OpCall =.. [Op,X,Y], { Z = OpCall }.

%% clp-based implementation of relational operators
bif_rop(Op,X,Y,true)  :-
  OpCall =.. [Op,X,Y], { OpCall }.
bif_rop(Op,X,Y,false) :-
  % get the negation NOp of Op
  memberchk((Op,NOp),[('=<','>'),('<','>='),('>=','<'),('>','=<')]),
  bif_rop(NOp,X,Y,true).

% assumption: T1 and T2 are ground terms (due to the calls to subtype)
numerical_literal_res(T1,T2,T3) :-
  T1 == T2,
  T3 = T1.
numerical_literal_res(T1,T2,T3) :-
  T1 \== T2, ( T1 == float ; T2 == float ),
  T3 = float.
numerical_literal_res(T1,T2,T3) :-
  T1 \== T2, ( T1 == number ; T2 == number ),
  T3 = number.

%% subtypes(types1,types2)
%% types1 and types2 are two lists of types of the same length and
%% forall i, types1[i] is a subtype of types2[i]
% ASSUMPTION: types2 is a list of elements in {number,atom}
subtypes([],[]).
subtypes([T1|T1s],[T2|T2s]) :-
  subtype(T1,T2),
  subtypes(T1s,T2s).

%% subtype(A,B)
%% A is a subtype of B
subtype(T,number) :- ( T == int ; T == float ).
subtype(T,T).

%% diftypes(types1,types2)
%% types1 and types2 are two lists of types that differ in at least one element
diftypes([T1|T1s],[T2|T2s]) :-
  subtype(T1,T2),
  diftypes(T1s,T2s).
diftypes([T1|_T1s],[T2|_T2s]) :-
  dif(T1,T2), \+ subtype(T1,T2). % the order of dif and subtype is relevant
% test cases for diftypes/2
% ?- diftypes([number,X],[number,atom]).
% dif(X, atom) ;
% false.
% ?- diftypes([int,X],[number,atom]).
% dif(X, atom) ;
% false.
% ?- diftypes([int,atom],[number,atom]).
% false.

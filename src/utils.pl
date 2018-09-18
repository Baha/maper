:- use_module(library(clpq)).

:- discontiguous bif/4.

fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).

%% var_binding(var,env,result,env1)
%% searches a variable var in environment env and returns its value val
%% if var belongs to env, otherwise it adds the pair (var,val) to env.
var_binding(Var,[],Val,[Bind]) :-
  Bind = (Var,Val).
var_binding(Var,[Bind|REnv1],Val,[Bind|REnv1]) :-
  Bind = (Var1,Val1),
  Var == Var1,
  Val = Val1.
var_binding(Var,[Bind|REnv1],Val,[Bind|Renv2]) :-
  Bind = (Var1,_Val1),
  Var \== Var1,
  var_binding(Var,REnv1,Val,Renv2).

%% zip_binds(vars,values,binds)
%% tuples a list of variables with their values
zip_binds([],[],[]).
zip_binds([var(Par)|RPars],[Val|Vals],[(Par,Val)|RPars1]) :-
  zip_binds(RPars,Vals,RPars1).

%% bif(mod,fun,inputs,outputs)
%% emulates the execution of a given BIF

% Term Comparisons -------------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#term-comparisons
bif(lit(atom,erlang),lit(atom,'=<'), [Lit1,Lit2], lit(atom,Z)) :-
  lte(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'<'),  [Lit1,Lit2], lit(atom,Z)) :-
  lt(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'>='), [Lit1,Lit2], lit(atom,Z)) :-
  gte(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'>'),  [Lit1,Lit2], lit(atom,Z)) :-
  gt(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'=='), [Lit1,Lit2], lit(atom,Z)) :-
  eq(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'/='), [Lit1,Lit2], lit(atom,Z)) :-
  neq(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'=:='),[Lit1,Lit2], lit(atom,Z)) :-
  eeq(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'=/='),[Lit1,Lit2], lit(atom,Z)) :-
  eneq(Lit1,Lit2,Z).

% =<
lte(lit(T1,X),lit(T2,Y),true) :- number_type(T1), number_type(T2), { X =< Y }.
lte(lit(T1,X),lit(T2,Y),true) :- T1 = atom, T2 = atom, X @=< Y.
lte(lit(T1,_),lit(T2,_),true) :- number_type(T1), T2 = atom.
lte(Lit1,Lit2,false) :- gt(Lit1,Lit2,true).
% <
lt(lit(T1,X),lit(T2,Y),true) :- number_type(T1), number_type(T2), { X < Y }.
lt(lit(T1,X),lit(T2,Y),true) :- T1 = atom, T2 = atom, X @< Y.
lt(lit(T1,_),lit(T2,_),true) :- number_type(T1), T2 = atom.
lt(Lit1,Lit2,false) :- gte(Lit1,Lit2,true).
% >=
gte(lit(T1,X),lit(T2,Y),true) :- number_type(T1), number_type(T2), { X >= Y }.
gte(lit(T1,X),lit(T2,Y),true) :- T1 = atom, T2 = atom, X @>= Y.
gte(lit(T1,_),lit(T2,_),true) :- T1 = atom, number_type(T2).
gte(Lit1,Lit2,false) :- lt(Lit1,Lit2,true).
% >
gt(lit(T1,X),lit(T2,Y),true) :- number_type(T1), number_type(T2), { X > Y }.
gt(lit(T1,X),lit(T2,Y),true) :- T1 = atom, T2 = atom, X @> Y.
gt(lit(T1,_),lit(T2,_),true) :- T1 = atom, number_type(T2).
gt(Lit1,Lit2,false) :- lte(Lit1,Lit2,true).
% ==
eq(lit(T1,X),lit(T2,Y),true) :- number_type(T1), number_type(T2), { X =:= Y }.
eq(lit(T1,X),lit(T2,Y),true) :- T1 = atom, T2 = atom, compare(=,X,Y).
eq(Lit1,Lit2,false) :- neq(Lit1,Lit2,true).
% =:=
eeq(lit(T1,X),lit(T2,Y),true) :- compare(=,T1,T2), compare(=,X,Y).
eeq(Lit1,Lit2,false) :- neeq(Lit1,Lit2,true).
% \=
neq(lit(T1,X),lit(T2,Y),true) :- number_type(T1), number_type(T2), { X =\= Y }.
neq(lit(T1,X),lit(T2,Y),true) :- T1 = atom, T2 = atom, dif(X,Y).
neq(lit(T1,_),lit(T2,_),true) :- T1 = atom, number_type(T2).
neq(lit(T1,_),lit(T2,_),true) :- number_type(T1), T2 = atom.
neq(Lit1,Lit2,false) :- eq(Lit1,Lit2,true).
% =\=
neeq(lit(T1,_),lit(T2,_),true) :- dif(T1,T2).
neeq(lit(T1,X),lit(T2,Y),true) :- compare(=,T1,T2), dif(X,Y).
neeq(Lit1,Lit2,false) :- qeq(Lit1,Lit2,true).

% Arithmetic Expressions -------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#arithmetic-expressions

%% binary arithmetic operations (+,-,*,/)
bif(lit(atom,erlang),lit(atom,Op),[Lit1,Lit2], Res) :-
  memberchk(Op,['+','-','*','/']),
  binary_arith_bif_res(Lit1,Op,Lit2, Res).

% result of binary arithmetic operations
binary_arith_bif_res(lit(T1,X),Op,lit(T2,Y), lit(T3,Z)) :-
  in1_in2_out_abif_types(T1,T2,T3),
  OpCall =.. [Op,X,Y], { Z = OpCall }.
binary_arith_bif_res(lit(T1,_),__,lit(T2,_), error(badarith)) :-
  in1_in2_abif_wrong_types(T1,T2).

% inputs/output types of arithmetic operations
in1_in2_out_abif_types(float,int,float).
in1_in2_out_abif_types(int,float,float).
in1_in2_out_abif_types(T,T,T) :-
  number_type(T).

% inputs/output types mismatch
in1_in2_abif_wrong_types(T1,_) :-
  dif(T1,int), dif(T1,float).
in1_in2_abif_wrong_types(_,T2) :-
  dif(T2,int), dif(T2,float).

%% unary arithmetic operations (+,-)
bif(lit(atom,erlang),lit(atom,Op),[Lit], Res) :-
  memberchk(Op,['+','-']),
  unary_arith_bif_res(Op,Lit, Res).

% result of unary arithmetic operations
unary_arith_bif_res(Op,lit(T,X), lit(T,Z)) :-
  number_type(T),
  OpCall =.. [Op,X], { Z = OpCall }.
unary_arith_bif_res(__,lit(T,_), error(badarith)) :-
  dif(T,int), dif(T,float).

%
number_type(float).
number_type(int).

:- use_module(library(clpq)).

:- discontiguous bif/6.

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

%% format_values(exp_in,exp_out)
%% puts exp_in in a list if it is a single expression
format_values(values(Exps),Exps).
format_values(Exp,[Exp]).

%% bif(mod,fun,inputs,outputs)
%% emulates the execution of a given BIF

% Term Comparisons -------------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#term-comparisons
bif(lit(atom,erlang),lit(atom,'=<'), ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  lte(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'<'),  ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  lt(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'>='), ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  gte(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'>'),  ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  gt(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'=='), ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  eq(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'/='), ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  neq(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'=:='),ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
  eeq(Lit1,Lit2,Z).
bif(lit(atom,erlang),lit(atom,'=/='),ErrF,[Lit1,Lit2], ErrF,lit(atom,Z)) :-
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
bif(lit(atom,erlang),lit(atom,Op),ErrF,[Lit1,Lit2], ErrF1,Res) :-
  memberchk(Op,['+','-','*','/']),
  binary_arith_bif_res(ErrF,Lit1,Op,Lit2, ErrF1,Res).

% result of binary arithmetic operations
binary_arith_bif_res(ErrF,lit(T1,X),Op,lit(T2,Y), ErrF,Res) :-
  in1_in2_out_abif_types(T1,T2,T3),
  OpCall =.. [Op,X,Y], { Z = OpCall },
  Res = lit(T3,Z).
binary_arith_bif_res(_,lit(T1,_),_,lit(T2,_), ErrF,Res) :-
  in1_in2_abif_wrong_types(T1,T2),
  ErrF = bot,
  Res = error(badarith).

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
bif(ErrF,lit(atom,erlang),lit(atom,Op),[Lit], ErrF1,Res) :-
  memberchk(Op,['+','-']),
  unary_arith_bif_res(ErrF,Op,Lit, ErrF1,Res).

% result of unary arithmetic operations
unary_arith_bif_res(ErrF,Op,lit(T,X), ErrF,Res) :-
  number_type(T),
  OpCall =.. [Op,X], { Z = OpCall },
  Res = lit(T,Z).
unary_arith_bif_res(ErrF,_,lit(T,_), ErrF1,Res) :-
  dif(T,int), dif(T,float),
  ErrF1 = bot,
  Res = error(badarith).

%
number_type(float).
number_type(int).

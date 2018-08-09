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

%% bif(mod,fun,inputs,outputs)
%% emulates the execution of a given BIF

% Term Comparisons -------------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#term-comparisons

%% '=:=' and '=/=' are exact comparisons (i.e., 2.0 =:= 2 is false)
bif(lit(atom,erlang),lit(atom,'=:='),[lit(_,X),lit(_,Y)], lit(atom,true)) :-
  \+ dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=:='),[lit(_,X),lit(_,Y)], lit(atom,false)) :-
  dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=/='),[lit(_,X),lit(_,Y)], lit(atom,true)) :-
  dif(X,Y).
bif(lit(atom,erlang),lit(atom,'=/='),[lit(_,X),lit(_,Y)], lit(atom,false)) :-
  \+ dif(X,Y).

%% number-number comparisons ('=<','<','>=','>','==','/=')
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,X),lit(T2,Y)], lit(atom,true)) :-
  number_type(T1), number_type(T2),
  memberchk((Op,ClpOp),
    [('=<','=<'),('<','<'),('>=','>='),('>','>'),('==','=:='),('/=','=\\=')]),
  OpCall =.. [ClpOp,X,Y], { OpCall }.
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,X),lit(T2,Y)], lit(atom,false)) :-
  number_type(T1), number_type(T2),
  memberchk((Op,NClpOp),
    [('=<','>'),('<','>='),('>=','<'),('>','=<'),('==','=\\='),('/=','=:=')]),
  OpCall =.. [NClpOp,X,Y], { OpCall }.

%% number-atom comparisons ('=<','<','>=','>','==','/=')
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,true)) :-
  memberchk(Op,['=<','<','=/=','/=']),
  number_type(T1), T2 = atom.
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,false)) :-
  memberchk(Op,['>=','>','==','=:=']),
  number_type(T1), T2 = atom.

%% atom-number comparisons ('=<','<','>=','>','==','/=')
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,false)) :-
  memberchk(Op,['=<','<','==','=:=']),
  T1 = atom, number_type(T2).
bif(lit(atom,erlang),lit(atom,Op),[lit(T1,_X),lit(T2,_Y)], lit(atom,true)) :-
  memberchk(Op,['>=','>','=/=','/=']),
  T1 = atom, number_type(T2).

% Arithmetic Expressions -------------------------------------------------------
% http://erlang.org/doc/reference_manual/expressions.html#arithmetic-expressions

%% binary arithmetic operations (+,-,*,/)
bif(lit(atom,erlang),lit(atom,Op),[L1,L2], Res) :-
  memberchk(Op,['+','-','*','/']),
  binary_arith_bif_res(L1,Op,L2,Res).

% result of binary arithmetic operations
binary_arith_bif_res(lit(T1,X),Op,lit(T2,Y),Res) :-
  in1_in2_out_abif_types(T1,T2,T3),
  OpCall =.. [Op,X,Y], { Z = OpCall },
  Res = lit(T3,Z).
binary_arith_bif_res(lit(T1,_),_,lit(_,_),Res) :-
  dif(T1,int), dif(T1,float),
  Res = error(badarith).
binary_arith_bif_res(lit(_,_),_,lit(T2,_),Res) :-
  dif(T2,int), dif(T2,float),
  Res = error(badarith).

% inputs/output types of arithmetic operations
in1_in2_out_abif_types(float,int,float).
in1_in2_out_abif_types(int,float,float).
in1_in2_out_abif_types(T,T,T) :-
  number_type(T).

%% unary arithmetic operations (+,-)
bif(lit(atom,erlang),lit(atom,Op),[Lit], Res) :-
  memberchk(Op,['+','-']),
  unary_arith_bif_res(Op,Lit,Res).

% result of unary arithmetic operations
unary_arith_bif_res(Op,lit(T,X),Res) :-
  number_type(T),
  OpCall =.. [Op,X], { Z = OpCall },
  Res = lit(T,Z).
unary_arith_bif_res(__,lit(T,_),Res) :-
  dif(T,int), dif(T,float),
  Res = error(badarith).

%
number_type(float).
number_type(int).

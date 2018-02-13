fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).

%% var_lookup(var,env,result)
%% searches a variable var in environment env and
%% returns its value, or undef if not found
var_lookup(_,[],undef).
var_lookup(Var,[(Var,Val)|_],Val).
var_lookup(Var,[(Var1,_)|REnv],Val) :-
  Var \== Var1,
  var_lookup(Var,REnv,Val).

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

%% type(term,type)
%% returns the type of term
type(lit(int(_)),int).
type(lit(atom(_)),atom).

%% types(terms,type)
%% returns the types of a list of terms
types([],[]).
types([Term|Terms],[Type|Types]) :-
  type(Term,Type),
  types(Terms,Types).

%% types(mod,fun,types)
%% returns the expected types for a given BIF
types(lit(atom(erlang)),lit(atom('+')),[int,int]).
types(lit(atom(erlang)),lit(atom('-')),[int,int]).
types(lit(atom(erlang)),lit(atom('*')),[int,int]).
types(lit(atom(erlang)),lit(atom('/')),[int,int]).
types(lit(atom(erlang)),lit(atom('==')),[int,int]).
types(lit(atom(erlang)),lit(atom('/=')),[int,int]).
types(lit(atom(erlang)),lit(atom('=<')),[int,int]).
types(lit(atom(erlang)),lit(atom('<')),[int,int]).
types(lit(atom(erlang)),lit(atom('>=')),[int,int]).
types(lit(atom(erlang)),lit(atom('>')),[int,int]).
types(lit(atom(erlang)),lit(atom('=:=')),[int,int]).
types(lit(atom(erlang)),lit(atom('=/=')),[int,int]).
types(lit(atom(erlang)),lit(atom('==')),[atom,atom]).
types(lit(atom(erlang)),lit(atom('/=')),[atom,atom]).
types(lit(atom(erlang)),lit(atom('=:=')),[atom,atom]).
types(lit(atom(erlang)),lit(atom('=/=')),[atom,atom]).
%% bif(mod,fun,inputs,outputs)
%% emulates the execution of a given BIF
bif(lit(atom(erlang)),lit(atom('+')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is X+Y.
bif(lit(atom(erlang)),lit(atom('-')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is X-Y.
bif(lit(atom(erlang)),lit(atom('*')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is X*Y.
bif(lit(atom(erlang)),lit(atom('/')),[lit(int(X)),lit(int(Y))], lit(int(Z))) :-
  Z is div(X,Y).

bif(lit(atom(erlang)),lit(atom('==')),[X,X], lit(atom(true))).
bif(lit(atom(erlang)),lit(atom('==')),[X,Y], lit(atom(false))) :-
  X \= Y.
bif(lit(atom(erlang)),lit(atom('/=')),[X,X], lit(atom(false))).
bif(lit(atom(erlang)),lit(atom('/=')),[X,Y], lit(atom(true))) :-
  X \= Y.
bif(lit(atom(erlang)),lit(atom('=<')),[lit(int(X)),lit(int(Y))], lit(atom(true))) :-
  X =< Y.
bif(lit(atom(erlang)),lit(atom('=<')),[lit(int(X)),lit(int(Y))], lit(atom(false))) :-
  not(X =< Y).
bif(lit(atom(erlang)),lit(atom('<')),[lit(int(X)),lit(int(Y))], lit(atom(true))) :-
  X < Y.
bif(lit(atom(erlang)),lit(atom('<')),[lit(int(X)),lit(int(Y))], lit(atom(false))) :-
  not(X < Y).
bif(lit(atom(erlang)),lit(atom('>=')),[lit(int(X)),lit(int(Y))], lit(atom(true))) :-
  X >= Y.
bif(lit(atom(erlang)),lit(atom('>=')),[lit(int(X)),lit(int(Y))], lit(atom(false))) :-
  not(X >= Y).
bif(lit(atom(erlang)),lit(atom('>')),[lit(int(X)),lit(int(Y))], lit(atom(true))) :-
  X > Y.
bif(lit(atom(erlang)),lit(atom('>')),[lit(int(X)),lit(int(Y))], lit(atom(false))) :-
  not(X > Y).
%% '=:=' and '=/=' are exact comparisons (i.e., 2.0 =:= 2 is false),
%% but we implement them as '==' and '/=' since we only consider integers
bif(lit(atom(erlang)),lit(atom('=:=')),[X,X], lit(atom(true))).
bif(lit(atom(erlang)),lit(atom('=:=')),[X,Y], lit(atom(false))) :-
  X \= Y.
bif(lit(atom(erlang)),lit(atom('=/=')),[X,X], lit(atom(false))).
bif(lit(atom(erlang)),lit(atom('=/=')),[X,Y], lit(atom(true))) :-
  X \= Y.

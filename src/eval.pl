:- module(eval,
    [ eval/3
    , prop_eval/3
    , run_prog/4
    ]).

:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.

:- dynamic eval_option/1.

user:file_search_path(erlang_module,ModulesDir) :-
  user:file_search_path(maper_src,Base),
  atom_concat(Base, '/modules',ModulesDir).
user:file_search_path(maper_src,'src').

:- use_module(library(lists)).
:- use_module(library(terms)).

:- use_module(erlang_module(erlang)). % BIFs
:- use_module(maper_src('match')).
:- include('utils').

:- discontiguous eval/3.

:- dynamic user:fundef/3.
:- multifile user:fundef/3.

%% run(mod,fun,args,final_env,final_exp)
%% loads mod and evaluates fun (from mod)
run_prog(Mod,(Fun,Arity),Args,FExp) :-
  retractall(user:fundef(_,_,_)),
  user:consult(Mod),
  run(Mod,Fun/Arity,Args,FExp).

%% eval(mod,fun,args,final_env,final_exp)
%% evaluates fun (from mod) application and
%% returns the final environment and expression
run(Mod,Fun/Arity,Args,FExp) :-
  user:fundef(lit(atom,Mod),fname(Fun,Arity),fun(Pars,_)),
  zip_binds(Pars,Args,Env),
  eval(apply(fname(Fun,Arity),Pars),Env,FExp).

%% (Error) ---------------------------------------------------------------------
eval(error(Reason),_Env,error(Reason)).

%% Values ----------------------------------------------------------------------
%% (Lit)
eval(lit(Type,Val),_Env,lit(Type,Val)).
%% (Var)
eval(var(Var),Env,Val) :-
  memberchk((Var,Val),Env).
%% (Tuple)
eval(tuple([]),_Env,tuple([])).
eval(tuple([Exp|Exps]),Env,tuple([FExp|FExps])) :-
  eval(Exp,Env,FExp),
  eval(tuple(Exps),Env,tuple(FExps)).
%% (Nil)
eval(nil,_Env,nil).
%% (List)
eval(cons(Exp,Exps),Env,cons(FExp,FExps)) :-
  eval(Exp,Env,FExp),
  eval(Exps,Env,FExps).
%% (AnonF)
eval(fun(Args,Body),Env,fun(Args,FBody)) :-
  replace_free_vars(Body,Env,FBody).

% evaluate a list of expressions
eval_list([],_Env,[]).
eval_list([Exp|Exps],Env,[FExp|FExps]) :-
  eval(Exp,Env,FExp),
  eval_list(Exps,Env,FExps).

%% (Seq) -----------------------------------------------------------------------
% returns the evaluation of the last expression
% Since any binding created by the sequence is transformed into a let-binding
% by cerl, the evaluation of Exp does not change the environment (bindings).
% Nevertheless, we cannot skip the evaluation of the single expressions in the
% sequence except the last one because they might have effects on the program
% execution, e.g., send)
eval(seq(Exp,Exps),Env,FExp) :-
  eval(Exp,Env,_Exp1),
  eval(Exps,Env,FExp).

%% (Block) ---------------------------------------------------------------------
eval(block([Exp]),Env,FExp) :-
  eval(Exp,Env,FExp).
eval(block([Exp|Exps]),Env,FExp) :-
  eval(Exp,Env,_),
  eval(block(Exps),Env,FExp).

%% (Let) -----------------------------------------------------------------------
eval(let([var(Var)],Expr1,Expr2),Env,Expr) :-
  eval(Expr1,Env,EExpr1),
  let_cont(Env,Var,EExpr1,Expr2, Expr).
% the evaluation of Expr1 succeeds
let_cont(Env,Var,EExpr1,Expr2,Expr) :-
  dif(EExpr1,error(_Reason)),
  eval(Expr2,[(Var,EExpr1)|Env],Expr).
% the evaluation of Expr1 fails
let_cont(_Env,_Var,EExpr1,_Expr2,EExpr1) :-
  EExpr1 = error(_Reason).

%% (Case) ----------------------------------------------------------------------
eval(case(CExps,Clauses),Env,Exp) :-
  eval_list(CExps,Env,EExps),
  suspend_on(Env,EExps,Clauses, Cond),
  when(Cond, ( match(Env,EExps,Clauses, MEnv,ClBody), eval(ClBody,MEnv,Exp) ) ).
% Cond is either a conjunction of the form ( nonvar(V1) , ..., nonvar(Vn) ), or
suspend_on(Env,Exps,Clauses, Cond) :-
  exists_pattern_on_compound_datatype(Env,Exps,Clauses, Cond),
  !.
% ground(goOn)
suspend_on(_Env,_Exps,_Clauses, Cond) :-
  Cond = ground(goOn).
% exists_pattern_on_compound_datatype(+Env,+Exps,+Clauses,-Cond)
% if there exists a clause Cl in Clauses matching Exps, and C binds V1,...,Vn
% to list or tuples, then Cond is of the form ( nonvar(V1) , ..., nonvar(Vn) )
exists_pattern_on_compound_datatype(Env,Exps,Clauses, Cond) :-
  term_variables(Exps,ExpsVars),
  copy_term((Env,Exps,ExpsVars),(CpyEnv,CpyExps,CpyExpsVars)),
  match(CpyEnv,CpyExps,Clauses, _MEnv,_ClBody),
  filter_bindings(ExpsVars,CpyExpsVars, Vars),
  vars2cond(Vars,Cond).
%
filter_bindings([],[], []).
filter_bindings([_|As],[T|Bs], Vs) :-
  var(T),
  filter_bindings(As,Bs, Vs).
filter_bindings([_|As],[T|Bs], Vs) :-
  nonvar(T), T = lit(_,_),
  filter_bindings(As,Bs, Vs).
filter_bindings([V|As],[T|Bs], [V|Vs] ) :-
  nonvar(T), ( T == nil ; T = cons(_,_) ), % lists
  filter_bindings(As,Bs, Vs).
filter_bindings([V|As],[T|Bs], [V|Vs] ) :-
  nonvar(T), T = tuple(_), % tuples
  filter_bindings(As,Bs, Vs).
%
vars2cond([V],nonvar(V)).
vars2cond([V|Vs], ( nonvar(V) , Cond ) ) :-
  vars2cond(Vs, Cond ).

%% (Apply) ---------------------------------------------------------------------
eval(apply(FName,IExps),Env,Exp) :-
  FName = fname(_,_), % named function
  % retrieve function definition (%TODO: Pass module here)
  user:fundef(lit(atom,_Module),FName,fun(Pars,FunBody)), 
  eval_list(IExps,Env,FExps),
  zip_binds(Pars,FExps,AppBinds),
  constrain_final_exp(FName,Exp),
  eval(FunBody,AppBinds,Exp).
eval(apply(AnonF,IExps),Env,Exp) :-
  AnonF = var(Var), % anonymous function
  % retrieve function definition from evn
  memberchk((Var,fun(Pars,FunBody)),Env),
  length(Pars,N), length(IExps,M),
  apply_anonf_cont(N,M,Pars,FunBody,IExps,Env,Exp).  
% eval utility predicate to add some constraints on the output expression
:- dynamic user:spec/2.
constrain_final_exp(Fun,Exp) :-
  eval_option(use_spec),
  Fun = var(FName,Arity),
  functor(Function,FName,Arity),
  user:spec(Function,OutSpec),
  memberchk(OutSpec,[non_neg_integer,pos_integer,neg_integer]),
  !,
  typeof(Exp,OutSpec).
constrain_final_exp(_Fun,_Exp).
% anonymous function utility predicate
apply_anonf_cont(N,M,Pars,FunBody,IExps,Env,Exp) :-
  M == N,
  eval_list(IExps,Env,FExps),
  zip_binds(Pars,FExps,AppBinds),
  eval(FunBody,AppBinds,Exp).
apply_anonf_cont(N,M,_Fun,_IExps,_Env,error(badarith(anonfun,N,M))) :-
  M \== N.
%% (Call) ----------------------------------------------------------------------
eval(call(lit(atom,erlang),lit(atom,Name),IExps),Env,Exp) :-
  call_cont(Name,IExps,Env,Exp).
%
eval(call(lit(atom,Module),lit(atom,Name),IExps),Env,Exp) :-
  Module \== erlang,
  user:ensure_loaded(erlang_module(Module)),
  length(IExps,Arity),
  eval(apply(var(Name,Arity),IExps),Env, Exp).
%
call_cont(Name,IExps,Env,Exp) :-
  erlang:bif_pred(Name/Arity),
  length(IExps,Arity),
  eval_list(IExps,Env,FExps),
  erlang:bif(Name,FExps,Exp).
call_cont(Name,IExps,_Env,error(undef)) :-
  erlang:bif_pred(Name/Arity),
  has_not_length(IExps,Arity).
call_cont(Name,_IExps,_Env,error(undef)) :-
  \+ exists_bif(Name).
%
exists_bif(Name) :-
  erlang:bif_pred(Name/_Arity).

%% (Primop) --------------------------------------------------------------------
eval(primop(lit(atom,match_fail),_),_Env,error(match_fail)).

%% (Try) -----------------------------------------------------------------------
eval(try(Arg,Vars,Body,EVars,Handler),Env,Exp) :-
  eval(Arg,Env,MExp),
  StdVarsBody = (Vars,Body),
  ErrVarsBody = (EVars,Handler),
  try_vars_body(Env,MExp,StdVarsBody,ErrVarsBody,Exp).

%% try_vars_body(init_env,mid_env,mid_exp,
%%               correct_case,error_case,
%%               final_env,final_exp)
%% auxiliar rule that returns final_env and final_exp
%% of a try-catch block depending on mid_env's error symbol
try_vars_body(_Env,MExp,_,(_ErrVars,ErrBody),ErrBody) :-
  MExp = error(_Reason).
try_vars_body(Env,MExp,(CVars,CBody),_,Exp) :-
  dif(MExp,error(_Reason)),
  ClauseExp = [clause(CVars,lit(atom,true),CBody)],
  CaseExp = case([MExp],ClauseExp),
  eval(CaseExp,Env,Exp).

% REVIEW: temporary code
prop_eval(_ExpIn,_Env,_ExpOut) :-
  true.
  %eval(ExpIn,Env,ExpOut).

fun_lookup(_,Fun,FunDef) :-
  fundef(_,Fun,FunDef).

var_lookup(_,[],undef).
var_lookup(Var,[(Var,Val)|_],Val).
var_lookup(Var,[(Var1,_)|REnv],Val) :-
  Var \== Var1,
  var_lookup(Var,REnv,Val).

init_binds([],[]).
init_binds([var(Par)|RPars],[(Par,_)|RPars1]) :-
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
zip_binds([var(Var)|Vars],[Val|Vals],[(Var,Val)|RBinds]) :-
  zip_binds(Vars,Vals,RBinds).

rename_fun(fun(Pars,Body),fun(RPars,RBody)) :-
  rename_pars(Pars,RPars,RDict),
  rename(RDict,Body,_,RBody).

rename_list(RDict,[Exp|Exps],FDict,[RExp|RExps]) :-
  rename(RDict,Exp,NDict,RExp),
  rename_list(NDict,Exps,FDict,RExps).

rename(RDict,Lit,RDict,Lit) :-
  Lit = lit(_).
rename(RDict,var(Var),RDict,var(NVar)) :-
  var_lookup(Var,RDict,NVar).
rename(RDict,var(Var),NDict,var(NVar)) :-
  var_lookup(Var,RDict,undef),
  rename_var(Var,NVar),
  append(RDict,[(Var,NVar)],NDict).
rename(RDict,cons(Hd,Tl),FDict,cons(NHd,NTl)) :-
  rename(RDict,Hd,NDict,NHd),
  rename(NDict,Tl,FDict,NTl).
rename(RDict,tuple(Elems),FDict,tuple(NElems)) :-
  rename_list(RDict,Elems,FDict,NElems).
rename(RDict,values(Elems),FDict,values(NElems)) :-
  rename_list(RDict,Elems,FDict,NElems).
rename(RDict,let(Vars,Arg,Body),FDict,let(NVars,NArg,NBody)) :-
  rename_list(RDict,Vars,NDict1,NVars),
  rename(NDict1,Arg,NDict2,NArg),
  rename(NDict2,Body,FDict,NBody).
rename(RDict,case(Arg,Clauses),FDict,case(NArg,NClauses)) :-
  rename(RDict,Arg,NDict,NArg),
  rename_list(NDict,Clauses,FDict,NClauses).
rename(RDict,clause(Pats,Guard,Body),FDict,clause(NPats,NGuard,NBody)) :-
  rename_list(RDict,Pats,NDict1,NPats),
  rename(NDict1,Guard,NDict2,NGuard),
  rename(NDict2,Body,FDict,NBody).
rename(RDict,apply(Op,Args),FDict,apply(NOp,NArgs)) :-
  rename(RDict,Op,NDict,NOp),
  rename_list(NDict,Args,FDict,NArgs).
rename(RDict,call(Mod,Name,Args),FDict,call(NMod,NName,NArgs)) :-
  rename(RDict,Mod,NDict1,NMod),
  rename(NDict1,Name,NDict2,NName),
  rename_list(NDict2,Args,FDict,NArgs).
rename(RDict,call(Mod,Name,Args),FDict,call(NMod,NName,NArgs)) :-
  rename(RDict,Mod,NDict1,NMod),
  rename(NDict1,Name,NDict2,NName),
  rename_list(NDict2,Args,FDict,NArgs).
rename(RDict,primop(Name,Args),FDict,primop(NName,NArgs)) :-
  rename(RDict,Name,NDict,NName),
  rename_list(NDict,Args,FDict,NArgs).

rename_pars([],[],[]).
rename_pars([var(Par)|RPars],[var(FreshPar)|FPars],[(Par,FreshPar)|RDict]) :-
  rename_var(Par,FreshPar),
  rename_pars(RPars,FPars,RDict).

rename_var(CurId,NewId) :-
  fresh_nums(Ls),
  last(Ls,N),
  N1 is N + 1,
  atom_number(N1Atom,N1),
  add_fresh(CurId,N1Atom,NewId),
  append(Ls,[N1],NewLs),
  retract(fresh_nums(Ls)),
  assertz(fresh_nums(NewLs)).

add_fresh(CurAtom,FreshN,FreshId) :-
  atom_concat(CurAtom,'_',AtomUnder),
  atom_concat(AtomUnder,FreshN,FreshId).

:- module(match, [match/5]).

:- use_module(library(clpfd)).

%% match(+IEnv,+Exps,+Cls, -OEnv,-OExp)
% matches a list of expressions Exps against a list of clauses Cls and returns
% the matching environment OEnv (IEnv with the new bindings from the variables
% occurring in the matching pattern), and the body of the matching clause OExp
match(IEnv,Exps,[Cl|Cls], OEnv,OExp) :-
  match_clause(IEnv,Exps,Cl, MEnv,MExp,Res),
  match_cont(Res,MEnv,MExp,Cls, OEnv,OExp).
%
match_cont(true,MEnv,MExp,_Cls, MEnv,MExp).
match_cont(false,IEnv,Exps,Cls, MEnv,MExp) :-
  match(IEnv,Exps,Cls, MEnv,MExp).

%% match_clause(+IEnv,+Exps,+Cl, -MEnv,-MExp,-Res)
% matches a list of expressions Exps against a clause Cl
% IF (Exps matches the guarded pattern of Cl), THEN Res = true,
% MEnv = [NEW|IEnv], where NEW is the list of the new bindings created
% by the pattern matching, and the body MExp of the matching clause Cl
% ELSE the initial environment IEnv and the initial expression Exps
match_clause(IEnv,Exps,clause(Pats,Guard,Body), MEnv,MExp,Res) :-
  match_pats(IEnv,IEnv,Exps,Pats, MEnv,PRes), % (*)
  match_guard(PRes,MEnv,Guard,Exps,Body, MExp,Res).
% (*) it takes the initial environment IEnv twice, the first is kept unmodified
% while the second is extended with the NEW bindings obtained from the matching

%% match_pats(+IEnv,+MEnv,+Exps,+Pats, -FEnv,-Res)
% matches a list of expressions Exps against a list of patterns Pats and
% IF (Exps matches the pattern Pats), THEN Res = true, FEnv = [NEW|IEnv]
% where NEW is the list of the new bindings created by match_expApat
match_pats(_IEnv,MEnv,[],[], MEnv,true).
match_pats(IEnv,MEnv,[Exp|Exps],[Pat|Pats], FEnv,Res) :-
  match_expApat(MEnv,Exp,Pat,NEnv), % MEnv is the accumulator of new bindings
  match_pats(IEnv,NEnv,Exps,Pats, FEnv,Res).
% ELSE Res = false, FEnv = IEnv. It discards the bindings that match_expApat
% might have created due to an expression that partially matches the pattern,
% (e.g, in matching the expression {42,hello} against the pattern {X,hi}, we
% get [('X',42)|IEnv], which is discarded due to the failure of match_expApat
% called with the arguments 'hello' and 'hi').
match_pats(IEnv,_MEnv,[Exp|_],[Pat|_], IEnv,false) :-
  mismatch(Exp,Pat).

%% match_expApat(+IEnv,+Exp,+Pat, -OEnv)
% matches an expression Exp against a pattern Pat. OEnv = [NEW|IEnv], where
% NEW is the list of the new bindings created by matching an expression
% against the variable var(Var) occurring in Pat
% literal
match_expApat(IEnv,lit(Type,Val),lit(Type,Val), IEnv).
% variable (assumption: all variables in pattern matching are fresh)
match_expApat(IEnv,Val,var(Var), [(Var,Val)|IEnv]).
% tuple
match_expApat(IEnv,tuple([]),tuple([]), IEnv).
match_expApat(IEnv,tuple([Exp|Exps]),tuple([Pat|Pats]), OEnv) :-
  match_expApat(IEnv,Exp,Pat, IEnv1),
  match_expApat(IEnv1,tuple(Exps),tuple(Pats), OEnv).
% cons
match_expApat(IEnv,nil,nil, IEnv).
match_expApat(IEnv,cons(Exp,Exps),cons(Pat,Pats), OEnv) :-
  match_expApat(IEnv,Exp,Pat, IEnv1),
  match_expApat(IEnv1,Exps,Pats, OEnv).
% alias
match_expApat(IEnv,Val,alias(var(Var),Pat), OEnv) :-
  match_expApat(IEnv,Val,var(Var), IEnv1),
  match_expApat(IEnv1,Val,Pat, OEnv).

%% mismatch(+Exp,+Pat)
% the expression Exp and the pattern Pat does not match
% literal
mismatch(Exp,lit(_Type,_Val)) :-
  when(nonvar(Exp),
    Exp \= lit(_,_)
).
mismatch(Exp,lit(Type1,Val)) :-
  erlang:bif('/=',[Exp,lit(Type1,Val)], lit(atom,true)).
% tuple
mismatch(Exp,tuple(_PatElems)) :-
  when(nonvar(Exp),
    Exp \= tuple(_ExpElems)
).
mismatch(Exp,tuple([])) :-
  when(nonvar(Exp), (
    Exp = tuple(ExpElems),
    dif(ExpElems,[]) )
).
mismatch(Exp,tuple([PatElem|PatElems])) :-
  when(nonvar(Exp), (
    Exp = tuple([ExpElem|ExpElems]),
    tuples_mismatch([ExpElem|ExpElems],[PatElem|PatElems]) )
).
% nil
mismatch(Exp,nil) :-
  dif(Exp,nil).
% cons
mismatch(Exp,cons(_HdPat,_TlPats)) :-
  when(nonvar(Exp),
    Exp \= cons(_HdExp,_TlExps)
).
mismatch(Exp,cons(HdPat,TlPats)) :-
  when(nonvar(Exp), (
    Exp = cons(HdExp,TlExps),
    lists_mismatch(cons(HdExp,TlExps),cons(HdPat,TlPats)) )
).
% alias
mismatch(Exp,alias(_PatVar,PatPat)) :-
  when(nonvar(Exp),
    mismatch(Exp,PatPat)
).

%% tuples_mismatch(?ExpElems,+PatElems)
% ExpElems and PatElems have a different length
tuples_mismatch(ExpElems,PatElems) :-
  diff_len(ExpElems,PatElems).
% there exists an expression in ExpElems that does not match with
% the corresponding element (same position in the list) in PatElems
tuples_mismatch(ExpElems,PatElems) :-
  same_len(ExpElems,PatElems),
  memberchk(var(_),PatElems), % optimization: exists a nonvar pattern
  exist_mismatch_elem(ExpElems,PatElems).

%% exist_mismatch_elem(?ExpElems,+PatElems)
% Exp and Pat does not match
exist_mismatch_elem([Exp|_Exps],[Pat|_Pats]) :-
  mismatch(Exp,Pat).
% Exp match with Pat
exist_mismatch_elem([Exp|Exps],[Pat|Pats]) :-
  match_expApat([],Exp,Pat, _MEnv),
  exist_mismatch_elem(Exps,Pats).

%% lists_mismatch(?ExpElems,+PatElems)
% HdExp and HdPat does not match
lists_mismatch(cons(HdExp,_TlExps),cons(HdPat,_TlPats)) :-
  mismatch(HdExp,HdPat).
% HdExp matches with HdPat and TlExps does not match with TlPats
lists_mismatch(cons(HdExp,TlExps),cons(HdPat,TlPats)) :-
  match_expApat([],HdExp,HdPat, _MEnv), % heads match
  mismatch(TlExps,TlPats).              % tails mismatch

%% match_guard(+Cond,+IEnv,+Guard,+Exps,+Body, -MExp,GRes)
% IF Cond = 'true', THEN evaluates the guard Guard in IEnv and returns
% either Exps or Body (see match_guard_ifte/4) according to the result
% GRes obatined from the evaluation of the guard, and GRes
match_guard(true,IEnv,Guard,Exps,Body, MExp,GRes) :-
  eval(Guard,IEnv,lit(atom,GRes)),
  match_guard_ifte(GRes,Exps,Body, MExp).
% ELSE return Exps and false
match_guard(false,_Env,_Guard,Exps,_Body, Exps,false).

%% match_guard_ifte(Cond,Exp1,Exp2,Exp)
% selects either Exps or Body according to the value of GRes
% IF 'true' (the guard is true in the matching environment),
% THEN Exp is the Body of the matching clause
match_guard_ifte(true,_Exps, Body, Body).
% ELSE Exp is the input expression
match_guard_ifte(false,Exps,_Body, Exps).

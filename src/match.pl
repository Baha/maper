:- module(match, [match/5]).

:- use_module(library(clpfd)).

%% match(+IEnv,+Exps,+Cls, -OEnv,-OExp)
%% matches a list of expressions Exps against a list of clauses Cls and returns
%% the matching environment OEnv (IEnv with the new bindings from the variables
%% occurring in the matching pattern), and the body of the matching clause OExp
match(IEnv,Exps,[Cl|Cls], OEnv,OExp) :-
  match_clause(IEnv,Exps,Cl, MEnv,MExp,Res),
  match_cont(Res,MEnv,MExp,Cls, OEnv,OExp).
%
match_cont(true,MEnv,MExp,_Cls, MEnv,MExp).
match_cont(false,IEnv,Exps,Cls, MEnv,MExp) :-
  match(IEnv,Exps,Cls, MEnv,MExp).

%% match_clause(+IEnv,+Exps,+Cl, -MEnv,-MExp,-Res)
%% matches a list of expressions Exps against a clause Cl
%% IF (Exps matches the guarded pattern of Cl), THEN Res = true,
%% MEnv = [NEW|IEnv], where NEW is the list of the new bindings created
%% by the pattern matching, and the body MExp of the matching clause Cl
%% ELSE the initial environment IEnv and the initial expression Exps
match_clause(IEnv,Exps,clause(Pats,Guard,Body), MEnv,MExp,Res) :-
  match_pats(IEnv,IEnv,Exps,Pats, MEnv,PRes), % (*)
  match_guard(PRes,MEnv,Guard,Exps,Body, MExp,Res).
% (*) it takes the initial environment IEnv twice, the first is kept unmodified
% while the second is extended with the NEW bindings obtained from the matching

%% match_pats(+IEnv,+MEnv,+Exps,+Pats, -FEnv,-Res)
%% matches a list of expressions Exps against a list of patterns Pats and
%% IF (Exps matches the pattern Pats), THEN Res = true, FEnv = [NEW|IEnv]
%% where NEW is the list of the new bindings created by match_expApat
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
%% matches an expression Exp against a pattern Pat. OEnv = [NEW|IEnv], where
%% NEW is the list of the new bindings created by matching an expression
%% against the variable var(Var) occurring in Pat
% literal
match_expApat(IEnv,lit(Type,Val),lit(Type,Val), IEnv).
% variable (assumption: all variables in pattern matching are fresh)
match_expApat(IEnv,Val,var(Var), [(Var,Val)|IEnv]).
% tuple
match_expApat(IEnv,tuple([]),tuple([]), IEnv).
match_expApat(IEnv,tuple([Exp|Exps]),tuple([Pat|Pats]), OEnv) :-
  match_expApat(IEnv,Exp,Pat, IEnv1),
  match_expApat(IEnv1,tuple(Exps),tuple(Pats), OEnv).
% list
% basis step for proper lists
match_expApat(IEnv,list([]),list([]), IEnv).
% basis step for lists whose tail is a variable
match_expApat(IEnv,list(Exps),list(var(V)), OEnv) :-
  match_expApat(IEnv,list(Exps),var(V), OEnv).
match_expApat(IEnv,list([Exp|Exps]),list([Pat|Pats]), OEnv) :-
  match_expApat(IEnv,Exp,Pat, IEnv1),
  match_expApat(IEnv1,list(Exps),list(Pats), OEnv).

%% mismatch(+Exp,+Pat)
%% the expression Exp and the pattern Pat does not match
% literal
mismatch(Exp,lit(_Type,_Val)) :-
  when(nonvar(Exp),
    Exp \= lit(_,_)
).
mismatch(Exp,lit(Type1,Val)) :-
  bif(lit(atom,erlang),lit(atom,'/='),[Exp,lit(Type1,Val)], lit(atom,true)).
% tuple
mismatch(Exp,tuple(_PatElems)) :-
  when(nonvar(Exp),
    Exp \= tuple(_ExpElems)
).
mismatch(Exp,tuple(PatElems)) :-
  when(nonvar(Exp), (
    Exp = tuple(ExpElems),
    mismatch_elems(ExpElems,PatElems) )
).
% list
mismatch(Exp,list(_PatElems)) :-
  when(nonvar(Exp),
    Exp \= list(_ExpElems)
).
mismatch(Exp,list(PatElems)) :-
  when(nonvar(Exp), (
    Exp = list(ExpElems),
    mismatch_elems(ExpElems,PatElems) )
).

%% mismatch_elems(?ExpElems,+PatElems)
% ExpElems and PatElems do not match if their lists of elements do not match
mismatch_elems(ExpElems,[]) :-
  dif(ExpElems,[]).
mismatch_elems(ExpElems,[Pat|Pats]) :-
  when(nonvar(ExpElems), (
    mismatch_elems_lists(ExpElems,[Pat|Pats])
  )
).

%% mismatch_elems_lists(?ExpElems,+PatElems)
% ExpElems and PatElems have a different length
mismatch_elems_lists(ExpElems,PatElems) :-
  diff_len(ExpElems,PatElems).
%  diff_length(ExpElems,PatElems).
% there exists an expression in ExpElems that does not match with
% the corresponding element (same position in the list) in PatElems
mismatch_elems_lists(ExpElems,PatElems) :-
  same_len(ExpElems,PatElems), exists_nonvar_pat(PatElems),
  exist_mismatch(ExpElems,PatElems).

%% exist_mismatch_plist(?ExpElems,+PatElems)
% Exp and Pat does not match
exist_mismatch([Exp|_Exps],[Pat|_Pats]) :-
  mismatch(Exp,Pat).
% Exp match with Pat and
exist_mismatch([Exp|Exps],[Pat|Pats]) :-
  % NOTE: Pat is nonvar, hence match_expApat is semidet
  match_expApat([],Exp,Pat, _MEnv),
  exist_mismatch(Exps,Pats).

%
exists_nonvar_pat([Pat|Pats]) :- Pat = var(_),
  exists_nonvar_pat(Pats).
exists_nonvar_pat([Pat|_Pats]) :- Pat \= var(_).

%% match_guard(+Cond,+IEnv,+Guard,+Exps,+Body, -MExp,GRes)
%% IF Cond = 'true', THEN evaluates the guard Guard in IEnv and returns
%% either Exps or Body (see match_guard_ifte/4) according to the result
%% GRes obatined from the evaluation of the guard, and GRes
match_guard(true,IEnv,Guard,Exps,Body, MExp,GRes) :-
  eval(Guard,IEnv,lit(atom,GRes)),
  match_guard_ifte(GRes,Exps,Body, MExp).
% ELSE return Exps and false
match_guard(false,_Env,_Guard,Exps,_Body, Exps,false).

% match_guard_ifte(Cond,Exp1,Exp2,Exp)
% selects either Exps or Body according to the value of GRes
% IF 'true' (the guard is true in the matching environment),
% THEN Exp is the Body of the matching clause
match_guard_ifte(true,_Exps, Body, Body).
% ELSE Exp is the input expression
match_guard_ifte(false,Exps,_Body, Exps).

:- include('utils').

%% match(init_env,init_exps,clauses,match_env,match_exp)
%% matches a list of expressions with a list of clauses and
%% returns the resulting environment and expression (clause body)
match(IEnv,IExps,_,IEnv,IExps) :-
  IEnv = (bot,_).
match(IEnv,IExps,Clauses,CEnv,CExp) :-
  IEnv = (top,_),
  match_list(IEnv,IExps,Clauses,CEnv,CExp).

match_list(Env,Exps,[Clause|_],CEnv,CExp) :-
  match_clause(Env,Exps,Clause,CEnv,CExp,true).      % (*)
match_list(Env,Exps,[Clause|Clauses],CEnv,CExp) :-
  match_clause(Env,Exps,Clause,_,_,false),           % (**)
  match_list(Env,Exps,Clauses,CEnv,CExp).
% EMA: general question: What happens CEnv when matching fail?

%% match_clause(init_env,init_exps,clause,match_env,match_exp,result)
%% matches a list of expressions with a clause and
%% returns the result (and environment and expression if true)
match_clause(IEnv,IExps,clause(Pats,Guard,Body),CEnv,Body,GRes) :-
  match_pats(IEnv,IExps,Pats,CEnv,true),
  match_guard(CEnv,Guard,GRes).
match_clause(IEnv,IExps,clause(Pats,_Guard,_Body1),CEnv,_Body2,false) :-
  match_pats(IEnv,IExps,Pats,CEnv,false).

%% match_pats(init_env,init_exps,patterns,match_env,result)
%% matches a list of expressions with a list of patterns and
%% returns the resulting environment if result is true
match_pats(Env,[],[],Env,true).
match_pats(Env,[Exp|Exps],[Pat|Pats],FEnv,Res) :-
  match_pat(Env,Exp,Pat,NEnv,true),
  match_pats(NEnv,Exps,Pats,FEnv,Res).
match_pats(Env,[Exp|_],[Pat|_],FEnv,false) :-
  match_pat(Env,Exp,Pat,FEnv,false).


match_pat(Env,lit(Type,Val),lit(Type,Val),Env,true).
match_pat(Env,tuple(ExpElems),tuple(PatElems),FEnv,true) :-
  match_pats(Env,ExpElems,PatElems,FEnv,true).
match_pat(Env,cons(ExpHead,ExpTail),cons(PatHead,PatTail),FEnv,true) :-
  match_pat(Env,ExpHead,PatHead,MEnv,true),
  match_pat(MEnv,ExpTail,PatTail,FEnv,true).
match_pat(Env,Val,var(Var),Env,true) :-
  Env = (_,Binds),
  var_lookup(Var,Binds,Val),
  Val \== undef.
match_pat(Env,Val,var(Var),NEnv,true) :-
  Env = (Error,Binds),
  var_lookup(Var,Binds,undef),
  append(Binds,[(Var,Val)],NBinds),  % Is the order of pairs relevant?
  NEnv = (Error,NBinds).             % If it is not, I would remove the append.
% EMA
%%%% ------------- possible alternative implementation -------------------------
%match_pat(Env,Val,var(Var),NEnv,true) :-
%  Env =  (Error,Binds),
%  var_lookup(Var,Binds,Val),
%  update_binds(Val,Var,Binds,NBinds), % find a suitable name for this predicate
%  NEnv = (Error,NBinds).
%
%update_binds(Val,_Var,Binds,NBinds) :-
%  Val \== undef, NBinds = Binds.
%update_binds(Val,Var,Binds,NBinds) :-
%  Val == undef, NBinds = [(Var,Val)|Binds].
%%%% ---------------------------------------------------------------------------
%match_pat(_,_,_,_,false).           % commented out by Ema.
match_pat(Env,Exp,Pat,NEnv,false) :- % What happens to NEnv ?
  unmatchable(Env,Exp,Pat),
  Env = NEnv.                        % Is it equal or not in case of mismatch?

% literals
unmatchable(_Env,Exp,Pat) :-
  Pat = lit(Type1,_Val1), Exp = lit(Type2,_Val2), % Pat and Exp are literals,
  dif(Type1,Type2).                               % of different type
unmatchable(_Env,Exp,Pat) :-
  Pat = lit(Type1,Val1), Exp = lit(Type2,Val2), % Pat and Exp are literals,
  Type1 = Type2,                                % of the same type
  dif(Val1,Val2).                               % representing different values
unmatchable(_Env,Exp,Pat) :-
  Pat =  lit(_,_), Exp \= lit(_,_), % Pat is a lit and Exp is not
  dif(Exp,Pat).
% tuples
unmatchable(_Env,Exp,Pat) :-
  Pat = tuple(_),  Exp \= tuple(_), % Pat is a tuple and Exp is not
  dif(Exp,Pat).
unmatchable(Env,Exp,Pat) :-
  Pat = tuple(T1), Exp = tuple(T2), % Pat and Exp are both tuples
  unmatchable_lists(Env,T2,T1),     % and their elements do not match
  dif(Exp,Pat).
% lists
unmatchable(_Env,Exp,Pat) :-
  Pat = cons(_,_), Exp \= cons(_,_), % Pat is a list and Exp is not
  dif(Exp,Pat).
unmatchable(Env,Exp,Pat) :-
  Pat = cons(_,_), Exp = cons(_,_),  % Pat are both lists
  cons_to_plList(Exp,Pat,ExpL,PatL),
  unmatchable_lists(Env,ExpL,PatL).  % and their elements do not match
% variables
unmatchable(Env,Exp,Pat) :-
  Pat = var(Var),
  Env = (_,Binds),
  var_lookup(Var,Binds,Val),
  Val \== undef,
  dif(Exp,Val).

% NOTE: same representation of lists?
% unmatchable_lists(env,list(expressions),list(patterns))
unmatchable_lists(_Env,[_|_],[]).
unmatchable_lists(_Env,[],[_|_]).
unmatchable_lists(Env,[E1|_],[E2|_]) :-
  match_pat(Env,E1,E2,_,false).
unmatchable_lists(Env,[E1|E1s],[E2|E2s]) :-
  match_pat(Env,E1,E2,NEnv,true),
  unmatchable_lists(NEnv,E1s,E2s).


% translate lists encoded using cons/2 to Prolog lists
cons_to_plList(cons(A1,B1),cons(A2,B2),[A1|T1],[A2|T2]) :-
  !,
  cons_to_plList(B1,B2,T1,T2).
cons_to_plList(B1,B2,B1,B2).


%% match_guard(env,exp,result)
%% returns true if guard evaluates to true
match_guard(Env,Guard,true) :-
  tr(cf(Env,Guard),cf(_,lit(atom,true))).
match_guard(Env,Guard,false) :-
  tr(cf(Env,Guard),cf(_,lit(atom,false))).
match_guard(Env,Guard,false) :-
  tr(cf(Env,Guard),cf((bot,_),_)). % bad_arg only?

-module(erl2fact).
-export([main/1]).

-include("include/maper_macros.hrl").

main(File) ->
  case compile:file(File, [to_core, binary]) of
    {ok, _ModuleName, CoreForms} ->
        FactDefs = cerl2fact_mod(CoreForms),
        %CoreName = atom_to_list(ModuleName) ++ ".txt",
        % file:write_file(CoreName,
        [io:fwrite("~s.~n~n", [FactDef]) || FactDef <- FactDefs];
    _ ->
      io:fwrite("Error: Could not compile file.~n"),
      exit(file_error)
  end.

cerl2fact_mod(Node) ->
  CoreModuleName = cerl:module_name(Node),
  CoreModuleDefs =
    [{FName, FDef} || {FName, FDef} <- cerl:module_defs(Node),
                     element(1,cerl:var_name(FName)) =/= 'module_info'],
  FactModuleDefs = [{CoreModuleName, FName, FDef} ||
                    {FName, FDef} <- CoreModuleDefs],
  FactFunDefs = [cerl2fact_fundef(FunDef) || FunDef <- FactModuleDefs],
  FactFunDefs.

cerl2fact_fundef({ModName, FunName, FunBody}) ->
  FactModuleName = cerl2fact(ModName),
  FactFunName    = cerl2fact(FunName),
  FactFunBody    = cerl2fact(FunBody),
  ?FUNDEF_PRED(FactModuleName, FactFunName, FactFunBody).

cerl2fact_lit([]) -> ?NIL_PRED;

cerl2fact_lit(Node) when is_integer(Node) ->
  FactInt = integer_to_list(Node),
  ?INT_PRED(FactInt);

cerl2fact_lit(Node) when is_atom(Node) ->
  FactAtom = atom_to_list(Node),
  ?ATOM_PRED(FactAtom).

cerl2fact(Node) when is_list(Node) ->
  FactList = [cerl2fact(CoreElem) || CoreElem <- Node],
  ?LIST_START ++ string:join(FactList, ?LIST_SEP) ++ ?LIST_END;

cerl2fact(Node) ->
  case catch(cerl:type(Node)) of
    'fun' ->
      CoreFunVars = cerl:fun_vars(Node),
      CoreFunBody = cerl:fun_body(Node),
      FactFunVars = cerl2fact(CoreFunVars),
      FactFunBody = cerl2fact(CoreFunBody),
      ?FUN_PRED(FactFunVars, FactFunBody);
    literal ->
      CoreConcrete = cerl:concrete(Node),
      case CoreConcrete of
        Tuple when is_tuple(Tuple) ->
          cerl2fact(lit2core(Tuple));
        List when is_list(List) ->
          cerl2fact(lit2core(List));
        _ ->
          FactConcrete = cerl2fact_lit(CoreConcrete),
          ?LIT_PRED(FactConcrete)
      end;
    var ->
      CoreVarName = cerl:var_name(Node),
      case CoreVarName of
        {ErlAtom,ErlInt} ->
          FactAtom = atom_to_list(ErlAtom),
          FactInt  = integer_to_list(ErlInt),
          ?VAR_PRED_PAIR(FactAtom,FactInt);
        _ ->
          FactVarName = atom_to_list(CoreVarName),
          ?VAR_PRED(FactVarName)
        end;
    cons ->
      CoreConsHead = cerl:cons_hd(Node),
      CoreConsTail = cerl:cons_tl(Node),
      FactConsHead = cerl2fact(CoreConsHead),
      FactConsTail = cerl2fact(CoreConsTail),
      ?CONS_PRED(FactConsHead, FactConsTail);
    tuple ->
      CoreTupleEs = cerl:tuple_es(Node),
      FactTupleEs = cerl2fact(CoreTupleEs),
      ?TUPLE_PRED(FactTupleEs);
    values ->
      CoreValuesEs = cerl:values_es(Node),
      FactValuesEs = cerl2fact(CoreValuesEs),
      ?VALUES_PRED(FactValuesEs);
    'let' ->
      CoreLetVars = cerl:let_vars(Node),
      CoreLetArg  = cerl:let_arg(Node),
      CoreLetBody = cerl:let_body(Node),
      FactLetVars = cerl2fact(CoreLetVars),
      FactLetArg  = cerl2fact(CoreLetArg),
      FactLetBody = cerl2fact(CoreLetBody),
      ?LET_PRED(FactLetVars, FactLetArg, FactLetBody);
    'case' ->
      CoreCaseArg = cerl:case_arg(Node),
      CoreCaseClauses = cerl:case_clauses(Node),
      FactCaseArg = cerl2fact(CoreCaseArg),
      FactCaseClauses = cerl2fact(CoreCaseClauses),
      ?CASE_PRED(FactCaseArg, FactCaseClauses);
    clause ->
      CoreClausePats = cerl:clause_pats(Node),
      CoreClauseGuard = cerl:clause_guard(Node),
      CoreClauseBody = cerl:clause_body(Node),
      FactClausePats = cerl2fact(CoreClausePats),
      FactClauseGuard = cerl2fact(CoreClauseGuard),
      FactClauseBody = cerl2fact(CoreClauseBody),
      ?CLAUSE_PRED(FactClausePats, FactClauseGuard, FactClauseBody);
    'apply' ->
      CoreApplyOp = cerl:apply_op(Node),
      CoreApplyArgs = cerl:apply_args(Node),
      FactApplyOp = cerl2fact(CoreApplyOp),
      FactApplyArgs = cerl2fact(CoreApplyArgs),
      ?APPLY_PRED(FactApplyOp, FactApplyArgs);          
    call ->
      CoreCallMod = cerl:call_module(Node),
      CoreCallName = cerl:call_name(Node),
      CoreCallArgs = cerl:call_args(Node),
      FactCallMod = cerl2fact(CoreCallMod),
      FactCallName = cerl2fact(CoreCallName),
      FactCallArgs = cerl2fact(CoreCallArgs),
      ?CALL_PRED(FactCallMod, FactCallName, FactCallArgs);
    primop ->
      CorePrimopName = cerl:primop_name(Node),
      CorePrimopArgs = cerl:primop_args(Node),
      FactPrimopName = cerl2fact(CorePrimopName),
      FactPrimopArgs = cerl2fact(CorePrimopArgs),
      ?PRIMOP_PRED(FactPrimopName, FactPrimopArgs);
    'try' ->
      CoreTryArg    = cerl:try_arg(Node),
      CoreTryVars    = cerl:try_vars(Node),
      CoreTryBody    = cerl:try_body(Node),
      CoreTryEVars   = cerl:try_evars(Node),
      CoreTryHandler = cerl:try_handler(Node),
      FactTryArg    = cerl2fact(CoreTryArg),
      FactTryVars    = cerl2fact(CoreTryVars),
      FactTryBody    = cerl2fact(CoreTryBody),
      FactTryEVars   = cerl2fact(CoreTryEVars),
      FactTryHandler = cerl2fact(CoreTryHandler),
      ?TRY_PRED(FactTryArg, FactTryVars, FactTryBody, FactTryEVars, FactTryHandler);
    _Literal ->
      cerl2fact(lit2core(Node))
  end.

lit2core(Lit) when is_integer(Lit) ->
  cerl:c_int(Lit);
lit2core(Lit) when is_atom(Lit) ->
  cerl:c_atom(Lit);
lit2core(Lit) when is_tuple(Lit) ->
  TupleEs = [element(I,Lit) || I <- lists:seq(1,tuple_size(Lit))],
  cerl:c_tuple(TupleEs);
lit2core(Lit) when is_list(Lit) ->
  cerl:c_values(Lit).
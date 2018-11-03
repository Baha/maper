-module(erl2fact).
-export([main/1]).

-include("../include/maper_macros.hrl").

main(File) ->
  case compile:file(File, [to_core, binary]) of
    {ok, _ModuleName, CoreForms} ->
        FactDefs = cerl2fact_mod(CoreForms),
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
        Float when is_float(Float) ->
          FloatStr = float_to_list(Float,[{decimals,15},compact]),
          ?LIT_PRED(?FLOAT_PRED(FloatStr));
        Int when is_integer(Int) ->
          IntStr = integer_to_list(Int),
          ?LIT_PRED(?INT_PRED(IntStr));
        Atom when is_atom(Atom) ->
          AtomStr = atom_to_list(Atom),
          ?LIT_PRED(?ATOM_PRED(AtomStr));
        Tuple when is_tuple(Tuple) ->
          TupleSize = tuple_size(Tuple),
          NewTuple = case TupleSize of
            0 -> ?LIST_START ++ ?LIST_END;
            N -> cerl2fact([element(I,Tuple) || I <- lists:seq(1,N)])
          end,
          ?TUPLE_PRED(NewTuple);
        List when is_list(List) ->
          ListLength = length(List),
          NewList = case ListLength of
            0 -> ?LIST_START ++ ?LIST_END;
            _ -> cerl2fact(List)
          end,
          ?LIST_PRED(NewList);
        Unsupported ->
          UStr = io_lib:format("~p",[Unsupported]),
          io:fwrite(standard_error,"~s~s~n",
            ["erl2fact: unsupported literal: ", UStr]),
          exit(unsupported_literal)
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
      case cerl:is_c_list(Node) of
        true ->
          ProperList = cerl2fact(cerl:list_elements(Node)),
          ?LIST_PRED(ProperList);
        false ->
          Head = cerl2fact(cerl:cons_hd(Node)),
          Tail = cerl2fact(cerl:cons_tl(Node)),
          ?CONS_PRED(Head, Tail)
      end;
    tuple ->
      CoreTupleEs = cerl:tuple_es(Node),
      FactTupleEs = cerl2fact(CoreTupleEs),
      ?TUPLE_PRED(FactTupleEs);
    values ->
      CoreValList = cerl:values_es(Node),
      cerl2fact(CoreValList);
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
      % Convert single case args to sequences to
      % avoid several cases in the intrepreter
      CoreValuesCaseArg =
        case cerl:is_c_values(CoreCaseArg) of
          true  -> CoreCaseArg;
          false -> cerl:c_values([CoreCaseArg])
        end,
      FactCaseArg = cerl2fact(CoreValuesCaseArg),
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
    seq ->
      Arg = cerl2fact(cerl:seq_arg(Node)),
      Seq = cerl2fact(cerl:seq_body(Node)),
      ?SEQ_PRED(Arg,Seq);
    %% Catch-all case for literal values
    _ ->
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

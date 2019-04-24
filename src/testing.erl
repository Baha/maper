-module(testing).
-export([main/2]).

main(FileName, PropName) ->
  put(free, 0),
  AFileName = list_to_atom(FileName),
  % PropFun = get_propfun(FileName, PropName),
  % io:format("~p~n", [PropFun]),
  comp_load(AFileName),
  {PropFun, _RestFuns} = get_propfun(FileName ++ ".erl", PropName),
  % io:format("~p~n", [PropFun]),
  FunClauses = erl_syntax:fun_expr_clauses(PropFun),
  FstClause = lists:nth(1,FunClauses),
  Pats = erl_syntax:clause_patterns(FstClause),
  PatStruct =
    case Pats of
      [{var,_,_}]    -> single;
      [{cons,_,_,_}] -> list;
      [{tuple,_,_}]  -> tuple
    end,
  % io:format("PATS: ~p~n", [Pats]),
  Vars = [free_named_var("TmpVar")],
  % Body = erl_syntax:clause_body(FstClause),
  Var = erl_syntax:variable("PropFun123"),
  Fun = erl_syntax:match_expr(Var, PropFun),
  % Call = erl_syntax:revert(erl_syntax:application(Var, Vars)),
  % Block = erl_syntax:block_expr([Fun,Call]),
  % io:format("~p~n", [Vars]),
  % io:format("~p~n", [Block]),
  read_lines(Vars, {Var,Fun}, FileName, PatStruct).

parse_inputs(Var, FLine) ->
  {ok, SLine, _} = erl_scan:string(FLine),
  {ok, Inputs} = erl_parse:parse_term(SLine),
  NInputs = [erl_parse:abstract(Inputs)],
  erl_syntax:application(Var, NInputs).

read_lines(Vars, {Var,Fun}, FileName, PatStruct) ->
  Line = io:get_line(""),
  case Line of
    eof ->
      io:format("~n"),
      ok;
    Line ->
      TLine = string:trim(Line),
      FLine =
        case PatStruct of
          single ->
            TLine ++ ".";
          list ->
            "[" ++ TLine ++ "].";
          tuple ->
            "{" ++ TLine ++ "}."
        end,
      Call = parse_inputs(Var,FLine),
      {ok, M1} = smerl:for_module(FileName ++ ".erl"),
      M2 = smerl:set_module(M1, prop_test),
      F = erl_syntax:revert(erl_syntax:function(erl_syntax:atom(foo),
        [erl_syntax:clause(none, [erl_syntax:block_expr([Fun,Call])])])),
      % io:format("~p~n", [F]),
      {ok, M3} = smerl:add_func(M2, F),
      smerl:compile(M3),
      % prop_test:foo(),
      try prop_test:foo() of
        true ->
          io:format(".");
        false ->
          io:format("x")
      catch
        _:_ ->
          io:format("c")
      end,
      read_lines(Vars, {Var,Fun}, FileName, PatStruct)
  end.

comp_load(FileName) ->
  % export_all required for non-exported funs
  compile:file(FileName, [export_all]),
  code:load_file(FileName).

get_propfun(FileName, PropName) ->
  Forms = forms:read(FileName),
  APropName = list_to_atom(PropName),
  FiltPred = fun (Form) ->
    try erl_syntax:type(Form) of
      function ->
        CFunName = erl_syntax:concrete(erl_syntax:function_name(Form)),
        case CFunName of
          APropName -> true;
          _         -> false
        end;
      _ -> false
    catch
      _:_ -> false
    end
  end,
  FiltRest = fun (Form) ->
    try erl_syntax:type(Form) of
      function ->
        CFunName = erl_syntax:concrete(erl_syntax:function_name(Form)),
        case CFunName of
          APropName -> false;
          _         -> true
        end;
      _ -> false
    catch
      _:_ -> false
    end
  end,
  Props = forms:filter(FiltPred, Forms),
  RestFuns = forms:filter(FiltRest, Forms),
  % Handle case when |Props| > 1
  TargetProp = lists:nth(1, Props),
  PropClauses = erl_syntax:function_clauses(TargetProp),
  ClauseBody = erl_syntax:clause_body(hd(PropClauses)),
  {call,_,{remote,_,{atom,_,_ProperMod},{atom,_,_ProperCall}},CallArgs} = hd(ClauseBody),
  PropFun = lists:nth(2,CallArgs),
  {PropFun, RestFuns}.

get_free() ->
  Free = get(free),
  put(free, Free + 1),
  Free.

free_named_var(NameRoot) ->
  erl_syntax:revert(erl_syntax:variable("_" ++ NameRoot ++ integer_to_list(get_free()))).

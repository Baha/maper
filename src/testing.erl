-module(testing).
-export([main/2]).

main(FileName, PropName) ->
  put(free, 0),
  AFileName = list_to_atom(FileName),
  % PropFun = get_propfun(FileName, PropName),
  % io:format("~p~n", [PropFun]),
  comp_load(AFileName),
  {PropFun, RestFuns} = get_propfun(FileName ++ ".erl", PropName),
  % io:format("~p~n", [PropFun]),
  FunClauses = erl_syntax:fun_expr_clauses(PropFun),
  FstClause = lists:nth(1,FunClauses),
  Pats = erl_syntax:clause_patterns(FstClause),
  NPats = case Pats of
    [{var,_,_}] ->
      Pats;
    _ ->
      fold_patterns(lists:nth(1,Pats))
  end,
  % io:format("NPATS: ~p~n", [NPats]),
  Vars = [free_named_var("TmpVar") || Pat <- NPats ],
  Body = erl_syntax:clause_body(FstClause),
  Var = erl_syntax:variable("PropFun123"),
  Fun = erl_syntax:match_expr(Var, PropFun),
  Call = erl_syntax:revert(erl_syntax:application(Var, Vars)),
  Block = erl_syntax:block_expr([Fun,Call]),
  % io:format("~p~n", [Vars]),
  % io:format("~p~n", [Block]),
  read_lines(Vars, Block, RestFuns).

fold_patterns({tuple,_,TupleEs}) -> TupleEs;
fold_patterns({cons, _, H, T}) -> [H|fold_patterns(T)];
fold_patterns({nil,_}) -> [];
fold_patterns(Pat) -> Pat.

read_lines(Vars, Call, Rest) ->
  Line = io:get_line(""),
  %% TODO: Replace eof case by "\n" or other cases
  case Line of
    eof ->
      io:format("finish~n");
    Line ->
      FLine =
        case length(Vars) of
          1 -> string:trim(Line) ++ ".";
          _ -> "[" ++ string:trim(Line) ++ "]."
        end,
      {ok, SLine, _} = erl_scan:string(FLine),
      {ok, Inputs} = erl_parse:parse_exprs(SLine),
      NInputs =
        case Inputs of
          [{tuple,_,_}] ->
            fold_patterns(lists:nth(1,Inputs));
          [{cons,_,_,_}] ->
            fold_patterns(lists:nth(1,Inputs));
          _ ->
            Inputs
        end,
      % io:format("INS: ~p~n", [NInputs]),
      MI = match_inputs(Vars, NInputs),
      % io:format("~p~n", [MI]),
      M1 = smerl:new(prop_test),
      F = erl_syntax:revert(erl_syntax:function(erl_syntax:atom(foo),
        [erl_syntax:clause(none, [MI, Call])])),
      % io:format("~p~n", [F]),
      % TODO: Add import proper attribute
      {ok, M2} = smerl:add_func(M1, F),
      M3 = add_rest(M2, Rest),
      smerl:compile(M3),
      % Add try-catch block to manage crashes
      Result = prop_test:foo(),
      io:format("~p~n", [Result]),
      read_lines(Vars, Call, Rest)
  end.

add_rest(M, []) -> M;
add_rest(M, [F|Fs]) ->
  {ok, M1} = smerl:add_func(M, F),
  add_rest(M1,Fs).


match_inputs(Vars, Inputs) ->
  ZipVI = lists:zip(Vars, Inputs),
  erl_syntax:revert(erl_syntax:block_expr([erl_syntax:match_expr(V,I) || {V,I} <- ZipVI])).

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
  erl_syntax:variable("_" ++ NameRoot ++ integer_to_list(get_free())).


-module(testing).
-export([main/2]).

main(FileName, PropName) ->
  put(free, 0),
  AFileName = list_to_atom(FileName),
  % PropFun = get_propfun(FileName, PropName),
  % io:format("~p~n", [PropFun]),
  comp_load(AFileName),
  PropFun = get_propfun(FileName ++ ".erl", PropName),
  io:format("~p~n", [PropFun]),

  FunClauses = erl_syntax:fun_expr_clauses(PropFun),
  FstClause = lists:nth(1,FunClauses),
  Vars = [free_named_var("TmpVar") || _ <- erl_syntax:clause_patterns(FstClause)],
  Body = erl_syntax:clause_body(FstClause),
  Var = erl_syntax:variable("PropFun123"),
  Fun = erl_syntax:match_expr(Var, PropFun),
  Call = erl_syntax:revert(erl_syntax:application(Var, Vars)),
  Block = erl_syntax:block_expr([Fun,Call]),
  io:format("~p~n", [Vars]),
  % io:format("~p~n", [Body]),
    io:format("~p~n", [Block]),
  read_lines(Vars, Block).

read_lines(Vars, Call) ->
  Line = io:get_line(""),
  %% TODO: Replace eof case by "\n" or other cases
  case Line of
    eof -> io:format("finish~n");
    Line ->
      FLine = "[" ++ string:trim(Line) ++ "].",
      {ok, SLine, _} = erl_scan:string(FLine),
      {ok, Inputs} = erl_parse:parse_exprs(SLine),
      MI = match_inputs(Vars, Inputs),
      io:format("~p~n", [MI]),
      M1 = smerl:new(prop_test),
      F = erl_syntax:revert(erl_syntax:function(erl_syntax:atom(foo),
        [erl_syntax:clause(none, [MI, Call])])),
      % RForms = erl_syntax:revert(F),
      io:format("~p~n", [F]),
      %% TODO:
      %%  1 .Add rest of funs so that they are defined
      %%  2. Solve shadowing problems (rename to random vars)
      {ok, M2} = smerl:add_func(M1, F),
      smerl:compile(M2),
      Result = prop_test:foo(),
      io:format("~p~n", [Result]),
      read_lines(Vars, Call)
  end.

match_inputs(Vars, Inputs) ->
  ZipVI = lists:zip(Vars, Inputs),
  erl_syntax:revert(erl_syntax:block_expr([erl_syntax:match_expr(V,I) || {V,I} <- ZipVI])).

comp_load(FileName) ->
  % export_all required for non-exported funs
  compile:file(FileName, [export_all]),
  io:format("~p~n", [code:load_file(FileName)]).

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

is_composite({cons,_,_,_}) -> true;
is_composite({tuple,_,_})  -> true;
is_composite(_) -> false.

convert_input(X) -> X.

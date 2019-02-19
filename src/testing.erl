-module(testing).
-export([main/2]).

main(FileName, PropName) ->
  AFileName = list_to_atom(FileName),
  % PropFun = get_propfun(FileName, PropName),
  % io:format("~p~n", [PropFun]),
  comp_load(AFileName),
  PropFun = get_propfun(FileName ++ ".erl", PropName),
  FunClauses = erl_syntax:fun_expr_clauses(PropFun),
  FstClause = lists:nth(1,FunClauses),
  Vars = erl_syntax:clause_patterns(FstClause),
  io:format("~p~n", [Vars]),
  read_lines(Vars).

read_lines(Vars) ->
  Line = io:get_line(""),
  case Line of
    eof -> io:format("finish~n");
    Line ->
      FLine = "[" ++ string:trim(Line) ++ "].",
      {ok, SLine, _} = erl_scan:string(FLine),
      {ok, Inputs} = erl_parse:parse_exprs(SLine),
      MI = match_inputs(Vars, Inputs),
      io:format("~p~n", [MI]),
      read_lines(Vars)
  end.

match_inputs(Vars, Inputs) ->
  ZipVI = lists:zip(Vars, Inputs),
  erl_syntax:block_expr([erl_syntax:match_expr(V,I) || {V,I} <- ZipVI]).

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
  Props = forms:filter(FiltPred, Forms),
  % Handle case when |Props| > 1
  TargetProp = lists:nth(1, Props),
  PropClauses = erl_syntax:function_clauses(TargetProp),
  ClauseBody = erl_syntax:clause_body(hd(PropClauses)),
  {call,_,{remote,_,{atom,_,_ProperMod},{atom,_,_ProperCall}},CallArgs} = hd(ClauseBody),
  PropFun = lists:nth(2,CallArgs).

% parse_input(Input, PropFun) ->
%   % check if PropFun expects composite

%   PropFormat = lists:nth(1, CallArgs),
%   io:format("~p~n", [CallArgs]),
%   PInput =
%     case is_composite(PropFormat) of
%       true ->
%         convert_input(Input);
%       false ->
%         Input
%     end,
%   PInput.

is_composite({cons,_,_,_}) -> true;
is_composite({tuple,_,_})  -> true;
is_composite(_) -> false.

convert_input(X) -> X.

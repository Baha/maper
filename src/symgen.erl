-module(symgen).
-export([main/1]).

main(File) ->
  Forms = forms:read(File),
  put(typenames,[]),
  Specs = forms:filter(fun is_spec/1, Forms),
  % io:format("Specs:~n~p~n", [Specs]),
  [generate_spec(S) || S <- Specs],
  UDTypes = forms:filter(fun is_type/1, Forms),
  % io:format("UDTYPES:~n~p~n", [UDTypes]),
  [generate_type(T) || T <- UDTypes],
  PropFuns = forms:filter(fun is_prop_fun/1, Forms),
  [generate_clauses(Fun) || Fun <- PropFuns].

is_prop_fun({function,_,_,0,FunClauses}) ->
  FirstClause = hd(FunClauses),
  Body = erl_syntax:clause_body(FirstClause),
  case hd(Body) of
    {call,_,{remote,_,{atom,_,proper},{atom,_,forall}},_} -> true;
    {call,_,{remote,_,{atom,_,proper_types},{atom,_,add_constraint}},_} -> true;
    {call,_,{remote,_,{atom,_,proper_types},{atom,_,bind}},_} -> true;
    _ -> false
  end;
is_prop_fun(_) -> false.

is_type({attribute,_,type,_}) -> true;
is_type({attribute,_, opaque,_}) -> true;
is_type(_) -> false.

is_spec({attribute,_,spec,_}) -> true;
is_spec(_) -> false.

generate_spec({attribute,_,spec,SpecT}) ->
  GenSpec = gen_spec_tuple(SpecT),
  io:format("~s~n~n", [GenSpec]).

gen_spec_tuple({FunName,FunIO}) ->
  gen_spec_data(FunName,FunIO).

gen_spec_data(FunName,FunIO) ->
  SpecName = gen_spec_name(FunName),
  SpecFun  = gen_spec_fun(SpecName, lists:nth(1,FunIO)),
  "spec(" ++ SpecFun ++ ").".

gen_spec_name({Name,_Arity}) ->
  atom_to_list(Name).

gen_spec_fun(Name, {type,_,'fun',[SpecProdInput,SpecOutput]}) ->
  {type,_,product,SpecInput} = SpecProdInput,
  GenSpecInput = [pp_type(T) || T <- SpecInput],
  StrSpecInput = string:join(GenSpecInput, ","),
  StrSpecOutput = pp_type(SpecOutput),
  Name ++ "(" ++ StrSpecInput ++ ")," ++ StrSpecOutput.
%% Access can be type (public) or opaque (private)
generate_type({attribute,_,_Access,TypeDef}) ->
  GenDef = generate_typedef(TypeDef),
  io:format("~s~n~n", [GenDef]).

generate_typedef({Name,Def,Args}) ->
  TypeNames = get(typenames),
  case lists:member(Name, TypeNames) of
    true ->
      ok;
    false ->
      put(typenames, [Name|TypeNames])
  end,
  TName = atom_to_list(Name),
  AName =
    case length(Args) of
      0 -> "";
      _ ->
        ArgsStr = [pp_type(A) || A <- Args],
        "(" ++ string:join(ArgsStr, ",") ++ ")"
    end,
  FName = TName ++ AName,
  generate_type_clauses(Def, FName).

generate_type_clauses({type,_,union,Defs},Name) ->
  ClausesStr = [generate_type_clauses(D,Name) || D <- Defs],
  string:join(ClausesStr, "\n");
generate_type_clauses(X, Name) ->
  "typedef(" ++ Name ++ "," ++ pp_type(X) ++ ").".
pp_type({type,_,tuple,TupleEs}) ->
  EsStr = [pp_type(E) || E <- TupleEs],
  "tuple([" ++ string:join(EsStr, ",")++ "])";
pp_type({user_type,_,Name,Args}) ->
  TName = atom_to_list(Name),
  AName =
    case length(Args) of
      0 -> "";
      _ ->
        ArgsStr = [pp_type(A) || A <- Args],
        "(" ++ string:join(ArgsStr, ",") ++ ")"
    end,
  TName ++ AName;
pp_type({type,_,Name,Args}) ->
  TName = atom_to_list(Name),
  AName =
    case length(Args) of
      0 -> "";
      _ ->
        ArgsStr = [pp_type(A) || A <- Args],
        "(" ++ string:join(ArgsStr, ",") ++ ")"
    end,
  TName ++ AName;
pp_type({var,_,Var}) -> atom_to_list(Var);
pp_type({atom,_,Atom}) -> "exactly(lit(atom," ++ atom_to_list(Atom) ++ "))".

generate_clauses(Fun) ->
  {function,_,_,0,Clauses} = Fun,
  {clause,_,_,_,Body} = hd(Clauses),
  {call,_,{remote,_,{atom,_,ProperMod},{atom,_,ProperCall}},CallArgs} = hd(Body),
  Types = lists:nth(1, CallArgs),
  Prop  = lists:nth(2, CallArgs),
  Vars = get_vars(Prop),
  PropFun = get_propfun(Prop),
  FunName = erl_syntax:function_name(Fun),
  GenName = forms:from_abstract(FunName),
  PropType =
    case {ProperMod, ProperCall} of
      {proper,forall} ->
        % io:format("FORALL (property)~n"),
        forall;
      {proper_types, add_constraint} ->
        % io:format("SUCHTHAT (user-defined type)~n"),
        user_cons;
      {proper_types, bind} ->
        % io:format("LET (user-defined type)~n"),
        user_bind
    end,
  ZipVT = zip_vars_types(Vars, [Types]),
  HeadStr  = pp_head(GenName, ZipVT),
  TypesStr = pp_vars_types(ZipVT),
  EvalStr = "eval(\n    " ++
    pp_propfun(PropFun) ++ ",\n    " ++
    pp_var_list(ZipVT)  ++ ",\n    " ++
    pp_prop_exp(PropType) ++"\n  )",
  PpStr =
    case PropType of
      forall ->
        HeadStr ++ TypesStr ++ ",\n  " ++ "prop_" ++ EvalStr;
      _ ->
        HeadStr ++ EvalStr ++ ",\n  " ++ TypesStr
    end,
  io:format("~s.~n~n", [PpStr]).

get_vars({'fun',_,{clauses,Clauses}}) ->
  FirstClause = hd(Clauses),
  {clause,_,Vars,_,_} = FirstClause,
  Vars.

get_propfun({'fun',_,{clauses,Clauses}}) ->
  FirstClause = hd(Clauses),
  [Body] = erl_syntax:clause_body(FirstClause),
  Body.

zip_vars_types([], []) -> [];
zip_vars_types([{tuple,_,VList}|RVars], [{tuple,_,TList}|RTypes]) ->
  zip_vars_types(VList,TList) ++ zip_vars_types(RVars,RTypes);
zip_vars_types([V|RVars],[T|RTypes]) ->
  [{V,T}] ++ zip_vars_types(RVars,RTypes).

pp_head(GenName, VarsTypes) ->
  VarsStr = [pp_var(V) || {V,_} <- VarsTypes],
  JointStr = string:join(VarsStr, ","),
  %HeadStr = "gen_" ++ GenName ++ "((" ++ JointStr ++ ")) :-\n  ",
  HeadStr = "gen_" ++ GenName ++ "(" ++ JointStr ++ ") :-\n  ",
  HeadStr.

pp_var(ConsVars = {cons,_,_,_}) ->
  ConsList = forms:cons_to_list(ConsVars),
  PpVars = [pp_var(V) || V <- ConsList],
  string:join(PpVars, ",");
pp_var({tuple,_,Vars}) ->
  PpVars = [pp_var(V) || V <- Vars],
  string:join(PpVars, ",");
pp_var({var,_,Var}) ->
  atom_to_list(Var).

pp_var_pair(ConsVars = {cons,_,_,_}) ->
  ConsList = forms:cons_to_list(ConsVars),
  PpVars = [pp_var_pair(V) || V <- ConsList],
  string:join(PpVars, ",");
pp_var_pair({tuple,_,Vars}) ->
  PpVars = [pp_var_pair(V) || V <- Vars],
  string:join(PpVars, ",");
pp_var_pair({var,_,Var}) ->
  VarStr = atom_to_list(Var),
  "('" ++ VarStr ++ "'," ++ VarStr ++ ")".

pp_var_list(VarsTypes) ->
  VarsStr = [pp_var_pair(V) || {V,_} <- VarsTypes],
  JointStr = string:join(VarsStr, ","),
  ListStr = "[" ++ JointStr ++ "]",
  ListStr.

pp_prop_exp(user_cons) -> "lit(atom,true)";
pp_prop_exp(user_bind) -> "lit(atom,true)";
pp_prop_exp(_) -> "lit(atom,_Res)".

pp_vars_types(VarsTypes) ->
  TypeofStr = [pp_vt(V,T) || {V,T} <- VarsTypes],
  string:join(TypeofStr, ",\n  ").

pp_vt({var,_,Var}, {call,_,{atom,_,CName},Args}) ->
  VarStr = atom_to_list(Var),
  CallStr = atom_to_list(CName),
  case is_predef(CName) of
    true ->
      ArgsStr =
        case length(Args) of
          0 -> "";
          _ -> "(" ++ pp_nested(Args) ++ ")"
        end,
      FullStr = "typeof(" ++ VarStr ++ "," ++ CallStr ++ ArgsStr ++ ")",
      FullStr;
    false ->
      Str = "gen_" ++ CallStr ++ "(" ++ VarStr ++ ")",
      Str
  end;

% Args will be empty in this case
pp_vt(Vars,{call,_,{atom,_,CName},_Args}) ->
  CallStr = atom_to_list(CName),
  ArgsStr = "(" ++ pp_var(Vars) ++ ")",
  GenStr = "gen_" ++ CallStr ++ "(" ++ ArgsStr ++ ")",
  GenStr.

% is_predef determines if type is predef or user-defined
% Non-supported cases will be correctly translated
is_predef(integer) -> true;
is_predef(float) -> true;
is_predef(atom) -> true;
is_predef(binary) -> true;
is_predef(bitstring) -> true;
is_predef(list) -> true;
is_predef(vector) -> true;
is_predef(union) -> true;
is_predef(weighted_union) -> true;
is_predef(tuple) -> true;
is_predef(loose_tuple) -> true;
is_predef(exactly) -> true;
is_predef(fixed_list) -> true;
is_predef(function) -> true;
is_predef(map) -> true;
is_predef(any) -> true;
is_predef(shrink_list) -> true;
is_predef(safe_union) -> true;
is_predef(safe_weighted_union) -> true;
is_predef(non_neg_integer) -> true;
is_predef(pos_integer) -> true;
is_predef(neg_integer) -> true;
is_predef(range) -> true;
is_predef(non_neg_float) -> true;
is_predef(number) -> true;
is_predef(boolean) -> true;
is_predef(byte) -> true;
is_predef(char) -> true;
is_predef(string) -> true;
is_predef(wunion) -> true;
is_predef(term) -> true;
is_predef(timeout) -> true;
is_predef(arity) -> true;
is_predef(resize) -> true;
is_predef(non_empty) -> true;
is_predef(noshrink) -> true;
is_predef(Name) ->
  TypeNames = get(typenames),
  lists:member(Name, TypeNames).

pp_nested([{call,_,{atom,_,CName},Args}]) ->
  CallStr = atom_to_list(CName),
  ArgsStr =
      case length(Args) of
      0 -> "";
      _ -> "(" ++ pp_nested(Args) ++ ")"
    end,
  CallStr ++ ArgsStr;
pp_nested(Args) ->
  FromAbsArgs = [forms:from_abstract(A) || A <- Args],
  string:join(FromAbsArgs, ",").

pp_propfun({block,_,List}) ->
  PpList = [pp_propfun(L) || L <- List],
  StrList = "[" ++ string:join(PpList, ",") ++ "]",
  "block(" ++ StrList ++ ")";
pp_propfun({call,_,Call,Args}) ->
  PpArgs = [pp_propfun(A) || A <- Args],
  StrArgs = "[" ++ string:join(PpArgs, ",") ++ "]",
  {CallPred, CallName} =
    case Call of
      {remote,_,_,_} -> {"call",pp_propfun(Call)};
      _ -> {"apply", pp_propfun(Call, length(Args))}
    end,
  CallPred ++ "(" ++ CallName ++ "," ++ StrArgs ++ ")";
pp_propfun({op,_,Op,Arg}) ->
  pp_propfun({call,0,{remote,0,{atom,0,erlang},{atom,0,Op}},[Arg]});
pp_propfun({op,_,Op,Arg1,Arg2}) ->
  pp_propfun({call,0,{remote,0,{atom,0,erlang},{atom,0,Op}},[Arg1,Arg2]});
pp_propfun({atom,_,Atom}) ->
  "lit(atom,'" ++ atom_to_list(Atom) ++ "')";
pp_propfun({remote,_,Mod,Fun}) ->
  pp_propfun(Mod) ++ "," ++ pp_propfun(Fun);
pp_propfun({var,_,Var}) ->
  "var('" ++ atom_to_list(Var) ++ "')";
pp_propfun({tuple,_,TupleEs}) ->
  PpEs = [pp_propfun(E) || E <- TupleEs],
  "tuple(" ++ string:join(PpEs, ",") ++ ")";
pp_propfun(_) -> "_".

pp_propfun({atom,_,Atom}, NArgs) ->
  "var('" ++ atom_to_list(Atom) ++ "'," ++ integer_to_list(NArgs) ++ ")".

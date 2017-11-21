-module(erl2pl).
-export([main/1]).

-include("include/maper_macros.hrl")

main(File) ->
  case compile:file(File, [to_core, binary, no_copt]) of
    {ok, ModuleName, CoreForms} ->
        TransForms = cerl_trees:map(fun translate/1, CoreForms),
        CoreName = atom_to_list(ModuleName) ++ ".txt",
        file:write_file(CoreName,
                        cerl_prettypr:format(TransForms));
    _ ->
      io:fwrite("Error: Could not compile file.~n"),
      exit(file_error)
  end.

translate(Node) ->
  case cerl:type(Node) of
    Node -> Node
    % cases
  end.
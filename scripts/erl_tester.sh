#!/usr/bin/env escript
%%! -pa ebin -smp enable -sname erl_tester

main([FileName,PropName]) ->
  testing:main(FileName, PropName).

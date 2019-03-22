fundef(lit(atom,'biggest_ok'),var('biggest',1),fun([var('@c0')],case([var('@c0')],[clause([cons(var('Head'),var('Tail'))],lit(atom,'true'),apply(var('biggest',2),[var('Tail'),var('Head')])),clause([var('@c1')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c1')])]))]))).

fundef(lit(atom,'biggest_ok'),var('biggest',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([nil,var('Biggest')],lit(atom,'true'),var('Biggest')),clause([cons(var('Head'),var('Tail')),var('Biggest')],call(lit(atom,'erlang'),lit(atom,'>='),[var('Head'),var('Biggest')]),apply(var('biggest',2),[var('Tail'),var('Head')])),clause([cons(var('Head'),var('Tail')),var('Biggest')],call(lit(atom,'erlang'),lit(atom,'<'),[var('Head'),var('Biggest')]),apply(var('biggest',2),[var('Tail'),var('Biggest')])),clause([var('@c3'),var('@c2')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c3'),var('@c2')])]))]))).

gen_prop_biggest((List)) :-
  typeof(List,non_empty(list(integer(2,5)))),
  prop_eval(
    block([call(lit(atom,'erlang'),lit(atom,'=:='),[apply(var('biggest',1),[var('List')]),call(lit(atom,'lists'),lit(atom,'last'),[call(lit(atom,'lists'),lit(atom,'sort'),[var('List')])])])]),
    [('List',List)],
    lit(atom,_Res)
  ).


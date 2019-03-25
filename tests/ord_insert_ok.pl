fundef(lit(atom,'ord_insert_ok'),var('is_ordered',1),fun([var('@c0')],case([var('@c0')],[clause([cons(var('A'),cons(var('B'),var('T')))],lit(atom,'true'),case([],[clause([],call(lit(atom,'erlang'),lit(atom,'=<'),[var('A'),var('B')]),apply(var('is_ordered',1),[cons(var('B'),var('T'))])),clause([],lit(atom,'true'),lit(atom,'false'))])),clause([var('@c4')],lit(atom,'true'),lit(atom,'true'))]))).

fundef(lit(atom,'ord_insert_ok'),var('ord_insert',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([var('I'),nil],lit(atom,'true'),cons(var('I'),nil)),clause([var('I'),cons(var('X'),var('Xs'))],call(lit(atom,'erlang'),lit(atom,'<'),[var('I'),var('X')]),cons(var('I'),cons(var('X'),var('Xs')))),clause([var('I'),cons(var('X'),var('Xs'))],lit(atom,'true'),let([var('@c2')],apply(var('ord_insert',2),[var('I'),var('Xs')]),cons(var('X'),var('@c2')))),clause([var('@c4'),var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c4'),var('@c3')])]))]))).

gen_ordered_list((List)) :-
  typeof(List,non_empty(list(integer))),
  eval(
    apply(var('is_ordered',1),[var('List')]),
    [('List',List)],
    lit(atom,true)
  ).

gen_prop_ordered_insert((Elem,List)) :-
  typeof(Elem,integer),
  gen_ordered_list(List),
  prop_eval(
    block([apply(var('is_ordered',1),[apply(var('ord_insert',2),[var('Elem'),var('List')])])]),
    [('Elem',Elem),('List',List)],
    lit(atom,_Res)
  ).


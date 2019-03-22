fundef(lit(atom,'sorted_ok'),var('is_ordered',1),fun([var('@c0')],case([var('@c0')],[clause([cons(var('A'),cons(var('B'),var('T')))],lit(atom,'true'),case([],[clause([],call(lit(atom,'erlang'),lit(atom,'=<'),[var('A'),var('B')]),apply(var('is_ordered',1),[cons(var('B'),var('T'))])),clause([],lit(atom,'true'),lit(atom,'false'))])),clause([var('@c4')],lit(atom,'true'),lit(atom,'true'))]))).

gen_prop_sort((List)) :-
  typeof(List,list(term)),
  prop_eval(
    block([apply(var('is_ordered',1),[call(lit(atom,'lists'),lit(atom,'sort'),[var('List')])])]),
    [('List',List)],
    lit(atom,_Res)
  ).


fundef(lit(atom,'delete_bug'),var('delete',2),fun([var('@c1'),var('@c0')],apply(var('delete',3),[var('@c1'),var('@c0'),nil]))).

fundef(lit(atom,'delete_bug'),var('delete',3),fun([var('@c2'),var('@c1'),var('@c0')],case([var('@c2'),var('@c1'),var('@c0')],[clause([var('@c7'),nil,var('Acc')],lit(atom,'true'),call(lit(atom,'lists'),lit(atom,'reverse'),[var('Acc')])),clause([var('X'),cons(var('@c8'),var('Rest')),var('Acc')],call(lit(atom,'erlang'),lit(atom,'=:='),[var('@c8'),var('X')]),let([var('@c3')],call(lit(atom,'lists'),lit(atom,'reverse'),[var('Acc')]),call(lit(atom,'erlang'),lit(atom,'++'),[var('@c3'),var('Rest')]))),clause([var('X'),cons(var('Y'),var('Rest')),var('Acc')],lit(atom,'true'),apply(var('delete',3),[var('X'),var('Rest'),cons(var('Y'),var('Acc'))])),clause([var('@c6'),var('@c5'),var('@c4')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c6'),var('@c5'),var('@c4')])]))]))).

gen_prop_delete((X,L)) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(var('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gen_prop_delete_with_stats((X,L)) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    apply(var('collect',2),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(var('delete',2),[var('X'),var('L')])])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gen_prop_delete_only_interesting1((X,L)) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    call(lit(atom,'proper'),lit(atom,'implies'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),_]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gen_list_and_elem1((X,L)) :-
  eval(
    call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),
    [('X',X),('L',L)],
    lit(atom,true)
  ),
  typeof(X,integer),
  typeof(L,list(integer)).

gen_prop_delete_only_interesting2((X,L)) :-
  gen_list_and_elem1((X,L)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(var('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gen_list_and_elem2((L)) :-
  eval(
    tuple(apply(var('oneof',1),[var('L')]),var('L')),
    [('L',L)],
    lit(atom,true)
  ),
  typeof(L,non_empty(list(integer))).

gen_prop_delete_only_interesting3((X,L)) :-
  gen_list_and_elem2((X,L)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(var('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

:- eval:assert(eval_option(use_spec)).

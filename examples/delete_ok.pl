:- style_check(-singleton).
fundef(lit(atom,'delete_ok'),fname('delete',2),fun([var('@c1'),var('@c0')],apply(fname('delete',3),[var('@c1'),var('@c0'),nil]))).

fundef(lit(atom,'delete_ok'),fname('delete',3),fun([var('@c2'),var('@c1'),var('@c0')],case([var('@c2'),var('@c1'),var('@c0')],[clause([var('@c6'),nil,var('Acc')],lit(atom,'true'),call(lit(atom,'lists'),lit(atom,'reverse'),[var('Acc')])),clause([var('X'),cons(var('@c7'),var('Rest')),var('Acc')],call(lit(atom,'erlang'),lit(atom,'=:='),[var('@c7'),var('X')]),apply(fname('delete',3),[var('X'),var('Rest'),var('Acc')])),clause([var('X'),cons(var('Y'),var('Rest')),var('Acc')],lit(atom,'true'),apply(fname('delete',3),[var('X'),var('Rest'),cons(var('Y'),var('Acc'))])),clause([var('@c5'),var('@c4'),var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c5'),var('@c4'),var('@c3')])]))]))).

fundef(lit(atom,'delete_ok'),fname('prop_delete',0),fun([],let([var('@c7')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c5')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c6')],call(lit(atom,'proper_types'),lit(atom,'list'),[var('@c5')]),let([var('@c4')],fun([var('@c2')],case([var('@c2')],[clause([tuple([var('X'),var('L')])],lit(atom,'true'),let([var('@c0')],apply(fname('delete',2),[var('X'),var('L')]),let([var('@c1')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('@c0')]),call(lit(atom,'erlang'),lit(atom,'not'),[var('@c1')])))),clause([var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c3')])]))])),call(lit(atom,'proper'),lit(atom,'forall'),[tuple([var('@c7'),var('@c6')]),var('@c4')]))))))).

fundef(lit(atom,'delete_ok'),fname('prop_delete_more',0),fun([],let([var('@c7')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c5')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c6')],call(lit(atom,'proper_types'),lit(atom,'list'),[var('@c5')]),let([var('@c4')],fun([var('@c2')],case([var('@c2')],[clause([tuple([var('X'),var('L')])],lit(atom,'true'),let([var('@c0')],apply(fname('delete',2),[var('X'),var('L')]),let([var('@c1')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('@c0')]),call(lit(atom,'erlang'),lit(atom,'not'),[var('@c1')])))),clause([var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c3')])]))])),let([var('@c8')],call(lit(atom,'proper'),lit(atom,'forall'),[tuple([var('@c7'),var('@c6')]),var('@c4')]),call(lit(atom,'proper'),lit(atom,'numtests'),[lit(int,1000),var('@c8')])))))))).

fundef(lit(atom,'delete_ok'),fname('prop_delete_with_stats',0),fun([],let([var('@c9')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c7')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c8')],call(lit(atom,'proper_types'),lit(atom,'list'),[var('@c7')]),let([var('@c6')],fun([var('@c4')],case([var('@c4')],[clause([tuple([var('X'),var('L')])],lit(atom,'true'),let([var('@c3')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),let([var('@c0')],apply(fname('delete',2),[var('X'),var('L')]),let([var('@c1')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('@c0')]),let([var('@c2')],call(lit(atom,'erlang'),lit(atom,'not'),[var('@c1')]),call(lit(atom,'proper'),lit(atom,'collect'),[var('@c3'),var('@c2')])))))),clause([var('@c5')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c5')])]))])),call(lit(atom,'proper'),lit(atom,'forall'),[tuple([var('@c9'),var('@c8')]),var('@c6')]))))))).

fundef(lit(atom,'delete_ok'),fname('prop_delete_only_interesting1',0),fun([],let([var('@c9')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c7')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c8')],call(lit(atom,'proper_types'),lit(atom,'list'),[var('@c7')]),let([var('@c6')],fun([var('@c4')],case([var('@c4')],[clause([tuple([var('X'),var('L')])],lit(atom,'true'),let([var('@c3')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),let([var('@c2')],fun([],let([var('@c0')],apply(fname('delete',2),[var('X'),var('L')]),let([var('@c1')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('@c0')]),call(lit(atom,'erlang'),lit(atom,'not'),[var('@c1')])))),call(lit(atom,'proper'),lit(atom,'implies'),[var('@c3'),var('@c2')])))),clause([var('@c5')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c5')])]))])),call(lit(atom,'proper'),lit(atom,'forall'),[tuple([var('@c9'),var('@c8')]),var('@c6')]))))))).

fundef(lit(atom,'delete_ok'),fname('list_and_elem1',0),fun([],let([var('@c5')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c3')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c4')],call(lit(atom,'proper_types'),lit(atom,'list'),[var('@c3')]),let([var('@c2')],fun([var('@c0')],case([var('@c0')],[clause([tuple([var('X'),var('L')])],lit(atom,'true'),call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')])),clause([var('@c1')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c1')])]))])),call(lit(atom,'proper_types'),lit(atom,'add_constraint'),[tuple([var('@c5'),var('@c4')]),var('@c2'),lit(atom,'true')]))))))).

fundef(lit(atom,'delete_ok'),fname('prop_delete_only_interesting2',0),fun([],let([var('@c5')],apply(fname('list_and_elem1',0),[]),let([var('@c4')],fun([var('@c2')],case([var('@c2')],[clause([cons(var('X'),cons(var('L'),nil))],lit(atom,'true'),let([var('@c0')],apply(fname('delete',2),[var('X'),var('L')]),let([var('@c1')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('@c0')]),call(lit(atom,'erlang'),lit(atom,'not'),[var('@c1')])))),clause([var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c3')])]))])),call(lit(atom,'proper'),lit(atom,'forall'),[var('@c5'),var('@c4')]))))).

fundef(lit(atom,'delete_ok'),fname('list_and_elem2',0),fun([],let([var('@c4')],call(lit(atom,'proper_types'),lit(atom,'integer'),[]),let([var('@c5')],call(lit(atom,'proper_types'),lit(atom,'list'),[var('@c4')]),let([var('@c6')],call(lit(atom,'proper_types'),lit(atom,'non_empty'),[var('@c5')]),let([var('@c3')],fun([var('@c1')],let([var('@c0')],call(lit(atom,'proper_types'),lit(atom,'oneof'),[var('@c1')]),tuple([var('@c0'),var('@c1')]))),call(lit(atom,'proper_types'),lit(atom,'bind'),[var('@c6'),var('@c3'),lit(atom,'false')]))))))).

fundef(lit(atom,'delete_ok'),fname('prop_delete_only_interesting3',0),fun([],let([var('@c5')],apply(fname('list_and_elem2',0),[]),let([var('@c4')],fun([var('@c2')],case([var('@c2')],[clause([tuple([var('X'),var('L')])],lit(atom,'true'),let([var('@c0')],apply(fname('delete',2),[var('X'),var('L')]),let([var('@c1')],call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('@c0')]),call(lit(atom,'erlang'),lit(atom,'not'),[var('@c1')])))),clause([var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c3')])]))])),call(lit(atom,'proper'),lit(atom,'forall'),[var('@c5'),var('@c4')]))))).

cg_prop_delete(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gc_prop_delete(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

cg_prop_delete_with_stats(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    apply(fname('collect',2),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gc_prop_delete_with_stats(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    apply(fname('collect',2),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

cg_prop_delete_only_interesting1(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    call(lit(atom,'proper'),lit(atom,'implies'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),_]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gc_prop_delete_only_interesting1(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  prop_eval(
    call(lit(atom,'proper'),lit(atom,'implies'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),_]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

cg_list_and_elem1(X,L) :-
  eval(
    call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),
    [('X',X),('L',L)],
    lit(atom,true)
  ),
  typeof(X,integer),
  typeof(L,list(integer)).

gc_list_and_elem1(X,L) :-
  typeof(X,integer),
  typeof(L,list(integer)),
  eval(
    call(lit(atom,'lists'),lit(atom,'member'),[var('X'),var('L')]),
    [('X',X),('L',L)],
    lit(atom,true)
  ).

cg_prop_delete_only_interesting2(X,L) :-
  cg_list_and_elem1(X,L),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gc_prop_delete_only_interesting2(X,L) :-
  gc_list_and_elem1(X,L),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

cg_list_and_elem2(L) :-
  eval(
    tuple(apply(fname('oneof',1),[var('L')]),var('L')),
    [('L',L)],
    lit(atom,true)
  ),
  typeof(L,non_empty(list(integer))).

gc_list_and_elem2(L) :-
  typeof(L,non_empty(list(integer))),
  eval(
    tuple(apply(fname('oneof',1),[var('L')]),var('L')),
    [('L',L)],
    lit(atom,true)
  ).

cg_prop_delete_only_interesting3(X,L) :-
  cg_list_and_elem2(X,L),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

gc_prop_delete_only_interesting3(X,L) :-
  gc_list_and_elem2(X,L),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[call(lit(atom,'lists'),lit(atom,'member'),[var('X'),apply(fname('delete',2),[var('X'),var('L')])])]),
    [('X',X),('L',L)],
    lit(atom,_Res)
  ).

:- set_config(int_inf(0)).
:- set_config(int_sup(10)).
:- set_config(start_size(3)).

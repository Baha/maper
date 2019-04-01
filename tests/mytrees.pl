fundef(lit(atom,'mytrees'),var('member',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([var('@c6'),lit(atom,'leaf')],lit(atom,'true'),lit(atom,'false')),clause([var('X'),tuple([lit(atom,'node'),var('@c7'),var('@c8'),var('@c9')])],call(lit(atom,'erlang'),lit(atom,'=:='),[var('@c7'),var('X')]),lit(atom,'true')),clause([var('X'),tuple([lit(atom,'node'),var('@c10'),var('L'),var('R')])],lit(atom,'true'),case([apply(var('member',2),[var('X'),var('L')])],[clause([lit(atom,'true')],lit(atom,'true'),lit(atom,'true')),clause([lit(atom,'false')],lit(atom,'true'),apply(var('member',2),[var('X'),var('R')])),clause([var('@c2')],lit(atom,'true'),call(lit(atom,'erlang'),lit(atom,'error'),[tuple([lit(atom,'badarg'),var('@c2')])]))])),clause([var('@c5'),var('@c4')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c5'),var('@c4')])]))]))).

fundef(lit(atom,'mytrees'),var('join',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([lit(atom,'leaf'),var('T2')],lit(atom,'true'),var('T2')),clause([tuple([lit(atom,'node'),var('X1'),var('L1'),var('R1')]),var('T2')],lit(atom,'true'),let([var('@c2')],apply(var('join',2),[var('L1'),var('R1')]),tuple([lit(atom,'node'),var('X1'),var('@c2'),var('T2')]))),clause([var('@c4'),var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c4'),var('@c3')])]))]))).

fundef(lit(atom,'mytrees'),var('delete',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([var('@c6'),lit(atom,'leaf')],lit(atom,'true'),lit(atom,'leaf')),clause([var('X'),tuple([lit(atom,'node'),var('@c7'),var('L'),var('R')])],call(lit(atom,'erlang'),lit(atom,'=:='),[var('@c7'),var('X')]),apply(var('join',2),[var('L'),var('R')])),clause([var('X'),tuple([lit(atom,'node'),var('Y'),var('L'),var('R')])],lit(atom,'true'),let([var('@c3')],apply(var('delete',2),[var('X'),var('L')]),let([var('@c2')],apply(var('delete',2),[var('X'),var('R')]),tuple([lit(atom,'node'),var('Y'),var('@c3'),var('@c2')])))),clause([var('@c5'),var('@c4')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c5'),var('@c4')])]))]))).

fundef(lit(atom,'mytrees'),var('tree_sum',1),fun([var('@c0')],case([var('@c0')],[clause([lit(atom,'leaf')],lit(atom,'true'),lit(int,0)),clause([tuple([lit(atom,'node'),var('X'),var('L'),var('R')])],lit(atom,'true'),let([var('@c2')],apply(var('tree_sum',1),[var('L')]),let([var('@c1')],apply(var('tree_sum',1),[var('R')]),let([var('@c3')],call(lit(atom,'erlang'),lit(atom,'+'),[var('@c2'),var('@c1')]),call(lit(atom,'erlang'),lit(atom,'+'),[var('@c3'),var('X')]))))),clause([var('@c4')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c4')])]))]))).

fundef(lit(atom,'mytrees'),var('preorder',1),fun([var('@c0')],case([var('@c0')],[clause([lit(atom,'leaf')],lit(atom,'true'),nil),clause([tuple([lit(atom,'node'),var('X'),var('L'),var('R')])],lit(atom,'true'),let([var('@c2')],apply(var('preorder',1),[var('L')]),let([var('@c1')],apply(var('preorder',1),[var('R')]),let([var('@c3')],call(lit(atom,'erlang'),lit(atom,'++'),[var('@c2'),var('@c1')]),cons(var('X'),var('@c3')))))),clause([var('@c4')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c4')])]))]))).

fundef(lit(atom,'mytrees'),var('tree1',1),fun([var('@c0')],let([var('@c2')],apply(var('tree1',1),[var('@c0')]),let([var('@c1')],apply(var('tree1',1),[var('@c0')]),call(lit(atom,'proper_types'),lit(atom,'union'),[cons(lit(atom,'leaf'),cons(tuple([lit(atom,'node'),var('@c0'),var('@c2'),var('@c1')]),nil))]))))).

fundef(lit(atom,'mytrees'),var('tree2',1),fun([var('@c0')],let([var('@c3')],fun([],let([var('@c2')],apply(var('tree2',1),[var('@c0')]),let([var('@c1')],apply(var('tree2',1),[var('@c0')]),tuple([lit(atom,'node'),var('@c0'),var('@c2'),var('@c1')])))),let([var('@c4')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c3')]),call(lit(atom,'proper_types'),lit(atom,'union'),[cons(lit(atom,'leaf'),cons(var('@c4'),nil))]))))).

fundef(lit(atom,'mytrees'),var('tree3',1),fun([var('@c0')],let([var('@c3')],fun([var('@c1')],apply(var('tree3',2),[var('@c1'),var('@c0')])),call(lit(atom,'proper_types'),lit(atom,'sized'),[var('@c3')])))).

fundef(lit(atom,'mytrees'),var('tree3',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([lit(int,0),var('@c11')],lit(atom,'true'),lit(atom,'leaf')),clause([var('S'),var('G')],lit(atom,'true'),let([var('@c2')],apply(var('tree3',2),[lit(int,0),var('G')]),let([var('@c7')],fun([],let([var('@c5')],call(lit(atom,'erlang'),lit(atom,'-'),[var('S'),lit(int,1)]),let([var('@c6')],apply(var('tree3',2),[var('@c5'),var('G')]),let([var('@c3')],call(lit(atom,'erlang'),lit(atom,'-'),[var('S'),lit(int,1)]),let([var('@c4')],apply(var('tree3',2),[var('@c3'),var('G')]),tuple([lit(atom,'node'),var('G'),var('@c6'),var('@c4')])))))),let([var('@c8')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c7')]),call(lit(atom,'proper_types'),lit(atom,'union'),[cons(var('@c2'),cons(var('@c8'),nil))])))))]))).

fundef(lit(atom,'mytrees'),var('tree4',1),fun([var('@c0')],let([var('@c3')],fun([var('@c1')],apply(var('tree4',2),[var('@c1'),var('@c0')])),call(lit(atom,'proper_types'),lit(atom,'sized'),[var('@c3')])))).

fundef(lit(atom,'mytrees'),var('tree4',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([lit(int,0),var('@c11')],lit(atom,'true'),lit(atom,'leaf')),clause([var('S'),var('G')],lit(atom,'true'),let([var('@c2')],apply(var('tree4',2),[lit(int,0),var('G')]),let([var('@c7')],fun([],let([var('@c5')],call(lit(atom,'erlang'),lit(atom,'-'),[var('S'),lit(int,1)]),let([var('@c6')],apply(var('tree4',2),[var('@c5'),var('G')]),let([var('@c3')],call(lit(atom,'erlang'),lit(atom,'-'),[var('S'),lit(int,1)]),let([var('@c4')],apply(var('tree4',2),[var('@c3'),var('G')]),tuple([lit(atom,'node'),var('G'),var('@c6'),var('@c4')])))))),let([var('@c8')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c7')]),call(lit(atom,'proper_types'),lit(atom,'frequency'),[cons(tuple([lit(int,1),var('@c2')]),cons(tuple([lit(int,9),var('@c8')]),nil))])))))]))).

fundef(lit(atom,'mytrees'),var('tree5',1),fun([var('@c0')],let([var('@c3')],fun([var('@c1')],apply(var('tree5',2),[var('@c1'),var('@c0')])),call(lit(atom,'proper_types'),lit(atom,'sized'),[var('@c3')])))).

fundef(lit(atom,'mytrees'),var('tree5',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([lit(int,0),var('@c11')],lit(atom,'true'),lit(atom,'leaf')),clause([var('S'),var('G')],lit(atom,'true'),let([var('@c2')],apply(var('tree5',2),[lit(int,0),var('G')]),let([var('@c7')],fun([],let([var('@c5')],call(lit(atom,'erlang'),lit(atom,'div'),[var('S'),lit(int,2)]),let([var('@c6')],apply(var('tree5',2),[var('@c5'),var('G')]),let([var('@c3')],call(lit(atom,'erlang'),lit(atom,'div'),[var('S'),lit(int,2)]),let([var('@c4')],apply(var('tree5',2),[var('@c3'),var('G')]),tuple([lit(atom,'node'),var('G'),var('@c6'),var('@c4')])))))),let([var('@c8')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c7')]),call(lit(atom,'proper_types'),lit(atom,'frequency'),[cons(tuple([lit(int,1),var('@c2')]),cons(tuple([lit(int,9),var('@c8')]),nil))])))))]))).

fundef(lit(atom,'mytrees'),var('tree',1),fun([var('@c0')],let([var('@c3')],fun([var('@c1')],apply(var('tree',2),[var('@c1'),var('@c0')])),call(lit(atom,'proper_types'),lit(atom,'sized'),[var('@c3')])))).

fundef(lit(atom,'mytrees'),var('tree',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([lit(int,0),var('@c14')],lit(atom,'true'),lit(atom,'leaf')),clause([var('S'),var('G')],lit(atom,'true'),let([var('@c2')],apply(var('tree',2),[lit(int,0),var('G')]),let([var('@c10')],fun([],let([var('@c6')],call(lit(atom,'erlang'),lit(atom,'div'),[var('S'),lit(int,2)]),let([var('@c7')],apply(var('tree',2),[var('@c6'),var('G')]),let([var('@c8')],call(lit(atom,'erlang'),lit(atom,'div'),[var('S'),lit(int,2)]),let([var('@c9')],apply(var('tree',2),[var('@c8'),var('G')]),let([var('@c5')],fun([var('@c3')],case([var('@c3')],[clause([cons(var('L'),cons(var('R'),nil))],lit(atom,'true'),tuple([lit(atom,'node'),var('G'),var('L'),var('R')])),clause([var('@c4')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c4')])]))])),call(lit(atom,'proper_types'),lit(atom,'bind'),[cons(var('@c7'),cons(var('@c9'),nil)),var('@c5'),lit(atom,'true')]))))))),let([var('@c11')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c10')]),call(lit(atom,'proper_types'),lit(atom,'frequency'),[cons(tuple([lit(int,1),var('@c2')]),cons(tuple([lit(int,9),var('@c11')]),nil))])))))]))).

spec(member(T,tree(T)),boolean).

spec(join(tree(T),tree(T)),tree(T)).

spec(delete(T,tree(T)),tree(T)).

spec(tree_sum(tree(number)),number).

spec(preorder(tree(T)),list(T)).

typedef(tree(T),exactly(lit(atom,leaf))).
typedef(tree(T),tuple([exactly(lit(atom,node)),T,tree(T),tree(T)])).

gen_prop_delete((X,T)) :-
  typeof(X,integer),
  typeof(T,tree(integer)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'not'),[apply(var('member',2),[var('X'),apply(var('delete',2),[var('X'),var('T')])])]),
    [('X',X),('T',T)],
    lit(atom,_Res)
  ).

gen_prop_sum((T)) :-
  typeof(T,tree(integer)),
  prop_eval(
    call(lit(atom,'erlang'),lit(atom,'=:='),[call(lit(atom,'lists'),lit(atom,'sum'),[apply(var('preorder',1),[var('T')])]),apply(var('tree_sum',1),[var('T')])]),
    [('T',T)],
    lit(atom,_Res)
  ).


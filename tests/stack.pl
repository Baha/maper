fundef(lit(atom,'stack'),var('is_empty',1),fun([var('@c0')],case([var('@c0')],[clause([tuple([lit(int,0),[]])],lit(atom,'true'),lit(atom,'true')),clause([tuple([var('_N'),cons(var('_Top'),var('_Rest'))])],lit(atom,'true'),lit(atom,'false')),clause([var('@c1')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c1')])]))]))).

fundef(lit(atom,'stack'),var('size',1),fun([var('@c0')],case([var('@c0')],[clause([tuple([var('N'),var('_Elems')])],lit(atom,'true'),var('N')),clause([var('@c1')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c1')])]))]))).

fundef(lit(atom,'stack'),var('push',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([var('X'),tuple([var('N'),var('Elems')])],lit(atom,'true'),let([var('@c2')],call(lit(atom,'erlang'),lit(atom,'+'),[var('N'),lit(int,1)]),tuple([var('@c2'),cons(var('X'),var('Elems'))]))),clause([var('@c4'),var('@c3')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c4'),var('@c3')])]))]))).

fundef(lit(atom,'stack'),var('pop',1),fun([var('@c0')],case([var('@c0')],[clause([tuple([lit(int,0),[]])],lit(atom,'true'),call(lit(atom,'erlang'),lit(atom,'throw'),[lit(atom,'stack_empty')])),clause([tuple([var('N'),cons(var('Top'),var('Rest'))])],call(lit(atom,'erlang'),lit(atom,'>'),[var('N'),lit(int,0)]),let([var('@c1')],call(lit(atom,'erlang'),lit(atom,'-'),[var('N'),lit(int,1)]),tuple([var('Top'),tuple([var('@c1'),var('Rest')])]))),clause([var('@c2')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c2')])]))]))).

fundef(lit(atom,'stack'),var('stack',1),fun([var('@c0')],let([var('@c3')],fun([var('@c1')],apply(var('stack',2),[var('@c1'),var('@c0')])),let([var('@c4')],call(lit(atom,'proper_types'),lit(atom,'sized'),[var('@c3')]),call(lit(atom,'proper_symb'),lit(atom,'well_defined'),[var('@c4')]))))).

fundef(lit(atom,'stack'),var('stack',2),fun([var('@c1'),var('@c0')],case([var('@c1'),var('@c0')],[clause([lit(int,0),var('@c20')],lit(atom,'true'),tuple([lit(atom,'$call'),lit(atom,'stack'),lit(atom,'new'),[]])),clause([var('S'),var('G')],lit(atom,'true'),let([var('@c2')],fun([],apply(var('stack',2),[lit(int,0),var('G')])),let([var('@c3')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c2')]),let([var('@c9')],fun([],let([var('@c7')],call(lit(atom,'erlang'),lit(atom,'-'),[var('S'),lit(int,1)]),let([var('@c8')],apply(var('stack',2),[var('@c7'),var('G')]),let([var('@c6')],fun([var('@c4')],case([var('@c4')],[clause([cons(var('St'),nil)],lit(atom,'true'),tuple([lit(atom,'$call'),lit(atom,'stack'),lit(atom,'push'),cons(var('G'),cons(var('St'),nil))])),clause([var('@c5')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c5')])]))])),call(lit(atom,'proper_types'),lit(atom,'bind'),[cons(var('@c8'),nil),var('@c6'),lit(atom,'true')]))))),let([var('@c10')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c9')]),let([var('@c16')],fun([],let([var('@c14')],call(lit(atom,'erlang'),lit(atom,'-'),[var('S'),lit(int,1)]),let([var('@c15')],apply(var('stack',2),[var('@c14'),var('G')]),let([var('@c13')],fun([var('@c11')],case([var('@c11')],[clause([cons(var('St'),nil)],lit(atom,'true'),tuple([lit(atom,'$call'),lit(atom,'erlang'),lit(atom,'element'),cons(lit(int,2),cons(tuple([lit(atom,'$call'),lit(atom,'stack'),lit(atom,'pop'),cons(var('St'),nil)]),nil))])),clause([var('@c12')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c12')])]))])),call(lit(atom,'proper_types'),lit(atom,'bind'),[cons(var('@c15'),nil),var('@c13'),lit(atom,'true')]))))),let([var('@c17')],call(lit(atom,'proper_types'),lit(atom,'lazy'),[var('@c16')]),call(lit(atom,'proper_types'),lit(atom,'frequency'),[cons(tuple([lit(int,1),var('@c3')]),cons(tuple([lit(int,5),var('@c10')]),cons(tuple([lit(int,5),var('@c17')]),nil)))]))))))))]))).

typedef(stack(T),tuple([non_neg_integer,list(T)])).

gen_prop_push_pop((X,S)) :-
  typeof(X,integer),
  typeof(S,stack(integer)),
  prop_eval(
    block([_,call(lit(atom,'erlang'),lit(atom,'=:='),[var('X'),var('Y')])]),
    [('X',X),('S',S)],
    lit(atom,_Res)
  ).


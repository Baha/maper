fundef(lit(atom,'sum_tuple'),var('main',2),fun([var('@c1'),var('@c0')],let([var('Tuple')],tuple([var('@c1'),var('@c0')]),apply(var('sum_tuple',1),[var('Tuple')])))).

fundef(lit(atom,'sum_tuple'),var('sum_tuple',1),fun([var('@c0')],case([var('@c0')],[clause([tuple([var('X'),var('Y')])],lit(atom,'true'),call(lit(atom,'erlang'),lit(atom,'+'),[var('X'),var('Y')])),clause([var('@c1')],lit(atom,'true'),primop(lit(atom,'match_fail'),[tuple([lit(atom,'function_clause'),var('@c1')])]))]))).


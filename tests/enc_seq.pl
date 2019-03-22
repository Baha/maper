fundef(lit(atom,'enc_seq'),var('main',1),fun([var('@c0')],apply(var('weird_seq',1),[var('@c0')]))).

fundef(lit(atom,'enc_seq'),var('weird_seq',1),fun([var('@c0')],seq(call(lit(atom,'erlang'),lit(atom,'+'),[var('@c0'),lit(int,7)]),seq(call(lit(atom,'erlang'),lit(atom,'+'),[lit(int,21),var('@c0')]),cons(var('@c0'),nil))))).

fundef(lit(atom,'enc_seq'),var('a_seq',1),fun([var('@c0')],let([var('Y')],call(lit(atom,'erlang'),lit(atom,'+'),[var('@c0'),lit(int,5)]),let([var('Z')],call(lit(atom,'erlang'),lit(atom,'+'),[lit(int,42),var('Y')]),cons(var('@c0'),cons(var('Z'),nil)))))).


fundef(lit(atom,'cases_primop'),var('main',1),fun([var('@c0')],let([var('@c1')],call(lit(atom,'erlang'),lit(atom,'+'),[var('@c0'),lit(int,2)]),case([var('@c0'),var('@c1')],[clause([lit(int,1),lit(int,3)],lit(atom,'true'),lit(atom,'ok')),clause([var('@f0'),var('@f1')],lit(atom,'true'),let([var('@c2')],tuple([var('@f0'),var('@f1')]),primop(lit(atom,'match_fail'),[tuple([lit(atom,'case_clause'),var('@c2')])])))])))).


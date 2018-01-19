cd tests
erl -noshell -pa ../ebin -run erl2fact main hello -s init stop #> ../src/tmp.pl
erl -noshell -pa ../ebin -run erl2fact main factorial -s init stop > ../src/tmp.pl
erl -noshell -pa ../ebin -run erl2fact main sum -s init stop #> ../src/tmp.pl
erl -noshell -pa ../ebin -run erl2fact main sum_tuple -s init stop

cd tests
erl -noshell -pa ../ebin -run erl2fact main sum -s init stop
erl -noshell -pa ../ebin -run erl2fact main sum_tuple -s init stop

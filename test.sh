cd tests
test_path="../src/"
cmd_init="erl -noshell -pa ../ebin -run erl2fact main "
cmd_end=" -s init stop"
for test in hello cases cases_primop factorial odd sum sum_tuple sum_list sign weird_fun; do
   $cmd_init$test$cmd_end > $test_path$test".pl"
done

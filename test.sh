cd tests
test_path="../src/"
cmd_init="erl -noshell -pa ../ebin -run erl2fact main "
cmd_end=" -s init stop"
for test in cases cases_primop cases_primop_tuple cases_primop_tuple_2 \
  enc_apply enc_list enc_seq enc_tuple \
  factorial foo hello odd sum sum_tuple sum_list sign weird_fun; do
     $cmd_init$test$cmd_end > $test".pl"
done

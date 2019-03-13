cd tests
test_path="../src/"
cmd_init="erl -noshell -pa ../ebin -run erl2fact main "
cmd_init2="erl -noshell -pa ../ebin -run symgen main "
cmd_end=" -s init stop"
for test in ord_insert_ok biggest_bug biggest_ok delete_bug mytrees sorted_ok stack \
  cases cases_primop cases_primop_tuple cases_primop_tuple_2 \
  enc_apply enc_list enc_seq enc_tuple \
  factorial foo hello odd sum sum_tuple sum_list sign weird_fun; do
     echo "Translating $test"
     cat $test_path"proper_types.pl" > $test".pl"
     echo >> $test".pl"
     $cmd_init$test$cmd_end >> $test".pl"
     $cmd_init2$test$cmd_end >> $test".pl"

done

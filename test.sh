for test in ord_insert_ok biggest_bug biggest_ok delete_bug mytrees sorted_ok stack \
  cases cases_primop cases_primop_tuple cases_primop_tuple_2 \
  enc_apply enc_list enc_seq enc_tuple \
  factorial foo hello odd sum sum_tuple sum_list sign weird_fun; do
     echo "Translating $test"
     ./erl2pl.sh tests/$test".erl"
done

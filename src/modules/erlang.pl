:- use_module(library(clpfd)).
:- use_module(library(clpr)).

:- discontiguous bif/4.

/** ----------------------------------------------------------------------------
 *  abs(Float) -> float()
 *  abs(Int) -> integer() >= 0
 *
 *    Types
 *      Int = integer()
 *
 *    Returns an integer or float that is the arithmetical absolute value of
 *    Float or Int, for example:
 *
 *    > abs(-3.33).
 *    3.33
 *    > abs(-3).
 *    3
 *
 *    Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'abs'),[lit(int,V)], lit(int,V)) :-
  V #>=0.
bif(lit(atom,erlang),lit(atom,'abs'),[lit(int,V)], lit(int,A)) :-
  V #=< -1, A #= -1*V.
bif(lit(atom,erlang),lit(atom,'abs'),[lit(float,V)], lit(float,V)) :-
  { V >=0 }.
bif(lit(atom,erlang),lit(atom,'abs'),[lit(float,V)], lit(float,A)) :-
  { V < 0, A =:= -1*V }.
bif(lit(atom,erlang),lit(atom,'abs'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) )).

/** ----------------------------------------------------------------------------
 *  erlang:append_element(Tuple1, Term) -> Tuple2
 *
 *    Types
 *      Tuple1 = Tuple2 = tuple()
 *      Term = term()
 *
 *    Returns a new tuple that has one element more than Tuple1, and
 *    contains the elements in Tuple1 followed by Term as the last
 *    element. Semantically equivalent to
 *    list_to_tuple(tuple_to_list(Tuple1) ++ [Term]), but much
 *    faster. Example:
 *
 *    > erlang:append_element({one, two}, three).
 *    {one,two,three}
 */
bif(lit(atom,erlang),lit(atom,'append_element'),[tuple(T1),E], tuple(T2)) :-
  append(T1,E,T2).
bif(lit(atom,erlang),lit(atom,'append_element'),[Arg1,_Arg2], error(badarg)) :-
  when(nonvar(Arg1), Arg1 \= tuple(_) ).

/** ----------------------------------------------------------------------------
 *  apply(Fun, Args) -> term()
 *
 *    Types
 *      Fun = function()
 *      Args = [term()]
 *
 *    Calls a fun, passing the elements in Args as arguments.

 *    If the number of elements in the arguments are known at compile
 *    time, the call is better written as Fun(Arg1, Arg2, ... ArgN).
 */
bif(lit(atom,erlang),lit(atom,'apply'),[FName,Args], Exp) :-
  eval(apply(FName,Args),[],Exp).

/** ----------------------------------------------------------------------------
 *  apply(Module, Function, Args) -> term()
 *
 *    Types
 *      Module = module()
 *      Function = atom()
 *      Args = [term()]
 *
 *    Returns the result of applying Function in Module to Args. The
 *    applied function must be exported from Module. The arity of the
 *    function is the length of Args. Example:
 *
 *    > apply(lists, reverse, [[a, b, c]]).
 *    [c,b,a]
 *    > apply(erlang, atom_to_list, ['Erlang']).
 *    "Erlang"
 *
 *    If the number of arguments are known at compile time, the call is
 *    better written as Module:Function(Arg1, Arg2, ..., ArgN).
 *
 *    Failure: error_handler:undefined_function/3 is called if the
 *    applied function is not exported. The error handler can be redefined
 *    (see process_flag/2). If error_handler is undefined, or if the user
 *    has redefined the default error_handler so the replacement module
 *    is undefined, an error with reason undef is generated.
 */
bif(lit(atom,erlang),lit(atom,'apply'),[_Mod,FName,Args], Exp) :-
  % TODO: Pass module here
  eval(apply(FName,Args),[],Exp).

/** ----------------------------------------------------------------------------
 *  atom_to_list(Atom) -> string()
 *
 *    Types
 *      Atom = atom()
 *
 *    Returns a string corresponding to the text representation of Atom, for
 *    example:
 *
 *    > atom_to_list('Erlang').
 *    "Erlang"
 */
bif(lit(atom,erlang),lit(atom,'atom_to_list'),[lit(atom,A)], L) :-
  when(nonvar(A), atom_to_list(A,L)).
bif(lit(atom,erlang),lit(atom,'atom_to_list'),[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= lit(atom,_)).

atom_to_list(Atom,List) :-
  atom_codes(Atom,PlList), pl2erl(PlList,List).

pl2erl([],lit(list,nil)).
pl2erl([H|T1],cons(H,T2)) :- pl2erl(T1,T2).

/** ----------------------------------------------------------------------------
 *  ceil(Number) -> integer()
 *
 *    Types
 *    Number = number()
 *
 *    Returns the smallest integer not less than Number. For example:
 *
 *    > ceil(5.5).
 *    6
 *
 *    Allowed in guard tests.
 %
 *    Test cases:
 *    > ceil(1.0).
 *    1
 *    > ceil(1.1).
 *    2
 *    > ceil(-1.0).
 *    -1
 *    > ceil(-1.1).
 *    -1
 *
 *    bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,1.0)], lit(int,C)).
 *    bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,1.1)], lit(int,C)).
 *    bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,-1.0)], lit(int,C)).
 *    bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,-1.1)], lit(int,C)).
 *
 *  https://proofwiki.org/wiki/Ceiling_of_Negative_equals_Negative_of_Floor
 */
bif(lit(atom,erlang),lit(atom,'ceil'),[lit(int,N)],   lit(int,N)).
bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,N)], lit(int,C)) :-
  { N >= 0, C >= N }, C #>= 0,
  when(nonvar(N), C is ceil(N)).
bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,N)], lit(int,C)) :-
  { N < 0, C >= N, M = -N }, C #=< -1, C #= -V,
  bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,M)], lit(int,V)).
bif(lit(atom,erlang),lit(atom,'ceil'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) )).

 /** ----------------------------------------------------------------------------
 *  erlang:delete_element(Index, Tuple1) -> Tuple2
 *
 *    Types
 *      Index = integer() >= 1
 *       1..tuple_size(Tuple1)
 *      Tuple1 = Tuple2 = tuple()
 *
 *    Returns a new tuple with element at Index removed from tuple
 *    Tuple1, for example:
 *
 *    > erlang:delete_element(2, {one, two, three}).
 *    {one,three}
 */
bif(lit(atom,erlang),lit(atom,'delete_element'),[lit(int,N),tuple(X)], tuple(Y)) :-
  when(nonvar(X), ( length(X,S), N #>= 1, N #=< S, del_elem(N,X,Y) ) ).
bif(lit(atom,erlang),lit(atom,'delete_element'),[lit(int,N),tuple(X)], error(badarg)) :-
  N #>= 1, when(nonvar(X), ( length(X,S), N #> S ) ).
bif(lit(atom,erlang),lit(atom,'delete_element'),[lit(int,N),_Arg2], error(badarg)) :-
  N #=< 0.
bif(lit(atom,erlang),lit(atom,'delete_element'),[lit(int,N),Arg2], error(badarg)) :-
  N #>= 1, when(nonvar(Arg2), Arg2 \= tuple(_) ).
bif(lit(atom,erlang),lit(atom,'delete_element'),[Arg1,_Arg2], error(badarg)) :-
  when(nonvar(Arg1), Arg1 \= lit(int,_) ).

del_elem(N,L1,L2) :-
  del_elem_(N,1,L1,L2).

del_elem_(N,N,[_|T1],T1).
del_elem_(N,M,[H|T1],[H|T2]) :-
  N \== M,
  L is M+1,
  del_elem_(N,L,T1,T2).

 /** ---------------------------------------------------------------------------
 *  element(N, Tuple) -> term()
 *
 *    Types
 *      N = integer() >= 1
 *       1..tuple_size(Tuple)
 *      Tuple = tuple()
 *
 *    Returns the Nth element (numbering from 1) of Tuple, for example:
 *
 *    > element(2, {a, b, c}).
 *    b
 *
 *  Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'element'),[lit(int,N),tuple(X)], Y) :-
  when(nonvar(X), ( length(X,S), N #>= 1, N #=< S, elem(N,X,Y) ) ).
bif(lit(atom,erlang),lit(atom,'element'),[lit(int,N),tuple(X)], error(badarg)) :-
  N #>= 1, when(nonvar(X), ( length(X,S), N #> S ) ).
bif(lit(atom,erlang),lit(atom,'element'),[lit(int,N),_Arg2], error(badarg)) :-
  N #=< 0.
bif(lit(atom,erlang),lit(atom,'element'),[lit(int,N),Arg2], error(badarg)) :-
  N #>= 1, when(nonvar(Arg2), Arg2 \= tuple(_)).
bif(lit(atom,erlang),lit(atom,'element'),[Arg1,_Arg2], error(badarg)) :-
  when(nonvar(Arg1), Arg1 \= lit(int,_) ).

elem(N,L,E) :-
  elem_(N,1,L,E).

elem_(N,N,[E|_],E).
elem_(N,M,[_|T],E) :-
  N \== M,
  L is M+1,
  elem_(N,L,T,E).

/** ----------------------------------------------------------------------------
 *  float(Number) -> float()
 *
 *    Types
 *      Number = number()
 *
 *    Returns a float by converting Number to a float, for example:
 *
 *    > float(55).
 *    55.0
 *
 *    Allowed in guard tests.
 *    Note
 *
 *    If used on the top level in a guard, it tests whether the argument is
 *    a floating point number; for clarity, use is_float/1 instead.
 *
 *    When float/1 is used in an expression in a guard, such as 'float(A) ==
 *    4.0', it converts a number as described earlier.
 */
bif(lit(atom,erlang),lit(atom,'float'),[lit(int,N)],   lit(float,F)) :-
  { F =:= N }.
bif(lit(atom,erlang),lit(atom,'float'),[lit(float,N)], lit(float,N)).
bif(lit(atom,erlang),lit(atom,'float'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) )).

/** ----------------------------------------------------------------------------
 *  float_to_list(Float) -> string()
 *
 *  Types
 *    Float = float()
 *
 *  The same as float_to_list(Float,[{scientific,20}]).
 */

/** ----------------------------------------------------------------------------
 *  float_to_list(Float, Options) -> string()
 *
 *  Types
 *    Float = float()
 *    Options = [Option]
 *    Option =
 *       {decimals, Decimals :: 0..253} |
 *       {scientific, Decimals :: 0..249} |
 *       compact
 *
 *    Returns a string corresponding to the text representation of Float
 *    using fixed decimal point formatting.
 *
 *    Available options:
 *
 *      * If option decimals is specified, the returned value contains
 *        at most Decimals number of digits past the decimal point. If the
 *        number does not fit in the internal static buffer of 256 bytes, the
 *        function throws badarg.
 *
 *      * If option compact is specified, the trailing zeros at the end of the
 *        list are truncated. This option is only meaningful together with
 *        option decimals.
 *
 *      * If option scientific is specified, the float is formatted using
 *        scientific notation with Decimals digits of precision.
 *
 *      * If Options is [], the function behaves as float_to_list/1.
 *
 *    Examples:
 *
 *    > float_to_list(7.12, [{decimals, 4}]).
 *    "7.1200"
 *    > float_to_list(7.12, [{decimals, 4}, compact]).
 *    "7.12"
 */

/** ----------------------------------------------------------------------------
 *  floor(Number) -> integer()
 *
 *    Types
 *      Number = number()
 *
 *    Returns the largest integer not greater than Number. For example:
 *
 *    > floor(-10.5).
 *    -11
 *
 *    Allowed in guard tests.
 *
 *    Test cases:
 *    > floor(1.0).
 *    1
 *    > floor(1.1).
 *    1
 *    > floor(-1.0).
 *    -1
 *    > floor(-1.1).
 *    -2
 *
 *    bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,1.0)], lit(int,C)).
 *    bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,1.1)], lit(int,C)).
 *    bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,-1.0)], lit(int,C)).
 *    bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,-1.1)], lit(int,C)).
 *
 *  https://proofwiki.org/wiki/Floor_of_Negative_equals_Negative_of_Ceiling
 */
bif(lit(atom,erlang),lit(atom,'floor'),[lit(int,N)],   lit(int,N)).
bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,N)], lit(int,F)) :-
  { N >= 0, F =< N }, F #>= 0,
  when(nonvar(N), ( V is floor(N), F #= V ) ).
bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,N)], lit(int,F)) :-
  { N < 0, F =< N, M = -N }, F #=< -1, F #= -V,
  bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,M)], lit(int,V)).
bif(lit(atom,erlang),lit(atom,'floor'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) )).

/** ----------------------------------------------------------------------------
 *  hd(List) -> term()
 *
 *  Types
 *    List = [term(), ...]
 *
 *  Returns the head of List, that is, the first element, for example:
 *
 *  > hd([1,2,3,4,5]).
 *  1
 *
 *  Allowed in guard tests.
 *
 *  Failure: badarg if List is the empty list [].
 */
bif(lit(atom,erlang),lit(atom,'hd'),[cons(H,_)], H).
bif(lit(atom,erlang),lit(atom,'hd'),[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= cons(_,_) ).

/** ----------------------------------------------------------------------------
 *  erlang:insert_element(Index, Tuple1, Term) -> Tuple2
 *    Types
 *      Index = integer() >= 1
 *       1..tuple_size(Tuple1) + 1
 *      Tuple1 = Tuple2 = tuple()
 *      Term = term()
 *
 *    Returns a new tuple with element Term inserted at position Index in
 *    tuple Tuple1. All elements from position Index and upwards are
 *    pushed one step higher in the new tuple Tuple2. Example:
 *
 *    > erlang:insert_element(2, {one, two, three}, new).
 *    {one,new,two,three}
*/
bif(lit(atom,erlang),lit(atom,'insert_element'),[lit(int,N),tuple(X),E], tuple(Y)) :-
  when(nonvar(X), ( length(X,S), N #>= 1, N #=< S, ins_elem(N,X,E,Y) ) ).
bif(lit(atom,erlang),lit(atom,'insert_element'),[lit(int,N),tuple(X),_E], error(badarg)) :-
  N #>= 1, when(nonvar(X), ( length(X,S), N #> S ) ).
bif(lit(atom,erlang),lit(atom,'insert_element'),[lit(int,N),_Arg2,_E], error(badarg)) :-
  N #=< 0.
bif(lit(atom,erlang),lit(atom,'insert_element'),[lit(int,N),Arg2,_E], error(badarg)) :-
  N #>= 1, when(nonvar(Arg2), Arg2 \= tuple(_)).
bif(lit(atom,erlang),lit(atom,'insert_element'),[Arg1,_Arg2,_E], error(badarg)) :-
  when(nonvar(Arg1), Arg1 \= lit(int,_) ).

ins_elem(N,L1,E,L2) :-
  ins_elem_(N,1,L1,E,L2).

ins_elem_(N,N,T1,E,[E|T1]).
ins_elem_(N,M,[H|T1],E,[H|T2]) :-
  N \== M,
  L is M+1,
  ins_elem_(N,L,T1,E,T2).

/** ----------------------------------------------------------------------------
 *  integer_to_list(Integer) -> string()
 *
 *    Types
 *      Integer = integer()
 *
 *    Returns a string corresponding to the text representation of Integer,
 *    for example:
 *
 *    > integer_to_list(77).
 *    "77"
 */
bif(lit(atom,erlang),lit(atom,'integer_to_list'),[lit(int,N)], L) :-
  when(nonvar(N), atom_to_list(N,L)).
bif(lit(atom,erlang),lit(atom,'integer_to_list'),[Term], error(badarg)) :-
  when(nonvar(Term), Term \= lit(int,_)).

/** ----------------------------------------------------------------------------
 *  integer_to_list(Integer, Base) -> string()
 *
 *    Types
 *      Integer = integer()
 *      Base = 2..36
 *
 *    Returns a string corresponding to the text representation of Integer
 *    in base Base, for example:
 *
 *    > integer_to_list(1023, 16).
 *    "3FF"
 */

/** ----------------------------------------------------------------------------
 *  is_atom(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is an atom, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'is_atom'),[lit(atom,_)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_atom'),[Term], lit(atom,false)) :-
  when(nonvar(Term), Term \= lit(atom,_)).

/** ----------------------------------------------------------------------------
 *  is_float(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a floating point number, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'is_float'),[lit(float,_)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_float'),[Term], lit(atom,false)) :-
  when(nonvar(Term), Term \= lit(float,_)).

/** ----------------------------------------------------------------------------
 *  is_function(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a fun, otherwise false.
 *
 *    Allowed in guard tests.
 */

/** ----------------------------------------------------------------------------
 *  is_function(Term, Arity) -> boolean()
 *
 *    Types
 *      Term = term()
 *      Arity = arity()
 *
 *    Returns true if Term is a fun that can be applied with Arity number
 *     of arguments, otherwise false.
 *
 *    Allowed in guard tests.
 */

/** ----------------------------------------------------------------------------
 *  is_integer(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is an integer, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'is_integer'),[lit(int,_)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_integer'),[Term], lit(atom,false)) :-
  when(nonvar(Term), Term \= lit(int,_)).

/** ----------------------------------------------------------------------------
 *  is_list(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a list with zero or more elements, otherwise
 *    false.
 *
 *    Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'is_list'),[lit(list,nil)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_list'),[cons(_Hd,_Tl)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_list'),[Term], lit(atom,false)) :-
  when(nonvar(Term), ( Term \= lit(list,nil), Term \= cons(_Hd,_Tl) ) ).

/** ----------------------------------------------------------------------------
 *  is_number(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is an integer or a floating point number.
 *    Otherwise returns false.
 *
 *    Allowed in guard tests.
*/
bif(lit(atom,erlang),lit(atom,'is_number'),[lit(int,_)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_number'),[lit(float,_)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_number'),[Term], lit(atom,false)) :-
  when(nonvar(Term), ( Term \= lit(int,_), Term \= lit(float,_) )).

/** ----------------------------------------------------------------------------
 *  is_tuple(Term) -> boolean()
 *
 *    Types
 *      Term = term()
 *
 *    Returns true if Term is a tuple, otherwise false.
 *
 *    Allowed in guard tests.
 */
bif(lit(atom,erlang),lit(atom,'is_tuple'),[tuple(_)], lit(atom,true)).
bif(lit(atom,erlang),lit(atom,'is_tuple'),[Arg], lit(atom,false)) :-
  when(nonvar(Arg), Arg \= tuple(_) ).

/** ----------------------------------------------------------------------------
 *  length(List) -> integer() >= 0
 *
 *    Types
 *      List = [term()]
 *
 *    Returns the length of List, for example:
 *
 *    > length([1,2,3,4,5,6,7,8,9]).
 *    9
*/
bif(lit(atom,erlang),lit(atom,'length'),[lit(list,nil)], lit(int,N)) :-
  N #= 0.
bif(lit(atom,erlang),lit(atom,'length'),[cons(H,T)], lit(int,N)) :-
  N #>= 1,
  when(nonvar(T), len(cons(H,T),N)).
bif(lit(atom,erlang),lit(atom,'length'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(list,nil), Arg \= cons(_,_) ) ).

len(cons(H,T),N) :-
  pl2erl(L,cons(H,T)),
  length(L,N).

/** ----------------------------------------------------------------------------
 *  max(Term1, Term2) -> Maximum
 *
 *    Types
 *      Term1 = Term2 = Maximum = term()
 *
 *    Returns the largest of Term1 and Term2. If the terms are equal, Term1
 *    is returned.
 */

/** ----------------------------------------------------------------------------
 *  min(Term1, Term2) -> Minimum
 *
 *    Types
 *      Term1 = Term2 = Minimum = term()
 *
 *    Returns the smallest of Term1 and Term2. If the terms are equal,
 *    Term1 is returned.
*/

/** ----------------------------------------------------------------------------
 *  round(Number) -> integer()
 *
 *    Types
 *      Number = number()
 *
 *    Returns an integer by rounding Number, for example:
 *
 *    > round(5.5).
 *    6
 *
 *    Allowed in guard tests.
 *
 *    Test cases:
 *    > round(3.0).
 *    3
 *    > round(3.5).
 *    4
 *    > round(3.4).
 *    3
 *    > round(-3.5).
 *    -4
 *    > round(-3.4).
 *    -3
 *
 *    bif(lit(atom,erlang),lit(atom,'round'),[lit(float,3.0)],lit(int,N)).
 *    bif(lit(atom,erlang),lit(atom,'round'),[lit(float,3.5)],lit(int,N)).
 *    bif(lit(atom,erlang),lit(atom,'round'),[lit(float,-3.5)],lit(int,N)).
 *    bif(lit(atom,erlang),lit(atom,'round'),[lit(float,-3.4)],lit(int,N)).
*/
bif(lit(atom,erlang),lit(atom,'round'),[lit(int,N)],   lit(int,N)).
bif(lit(atom,erlang),lit(atom,'round'),[lit(float,R)], lit(int,N)) :-
  { R >= 0, N - R =< 0.5, R < N },
  when(nonvar(R), N is round(R)).
bif(lit(atom,erlang),lit(atom,'round'),[lit(float,R)], lit(int,N)) :-
  { R >= 0, N =< R, R - N < 0.5 },
  when(nonvar(R), N is round(R)).
bif(lit(atom,erlang),lit(atom,'round'),[lit(float,R)], lit(int,N)) :-
  { R < 0, P = -R }, N #= -S,
  bif(lit(atom,erlang),lit(atom,'round'),[lit(float,P)], lit(int,S)).
bif(lit(atom,erlang),lit(atom,'round'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) )).

/** ----------------------------------------------------------------------------
 *  setelement(Index, Tuple1, Value) -> Tuple2
 *
 *    Types
 *      Index = integer() >= 1
 *        1..tuple_size(Tuple1
 *      Tuple1 = Tuple2 = tuple()
 *      Value = term()
 *
 *    Returns a tuple that is a copy of argument Tuple1 with the element
 *    specified by integer argument Index (the first element is the element
 *    with index 1) replaced by argument Value, for example:
 *
 *    > setelement(2, {10, green, bottles}, red).
 *    {10,red,bottles}
*/


/** ----------------------------------------------------------------------------
 *  size(Item) -> integer() >= 0
 *
 *    Types
 *      Item = tuple() | binary()
 *
 *    Returns the number of elements in a tuple or the number of bytes in a
 *    binary or bitstring, for example:
 *
 *    > size({morni, mulle, bwange}).
 *    3
 *    > size(<<11, 22, 33>>).
 *    3
 *
 *    For bitstrings, the number of whole bytes is returned. That is, if the
 *    number of bits in the bitstring is not divisible by 8, the resulting
 *    number of bytes is rounded down.
 *
 *    Allowed in guard tests.
 *
 *    See also tuple_size/1, byte_size/1, and bit_size/1.
*/
bif(lit(atom,erlang),lit(atom,'size'),[tuple(T)], S) :-
  bif(lit(atom,erlang),lit(atom,'tuple_size'),[tuple(T)], S).
%TODO: to add support for binary()
bif(lit(atom,erlang),lit(atom,'size'),[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= tuple(_) ).

/** ----------------------------------------------------------------------------
 *  tl(List) -> term()
 *
 *    Types
 *      List = [term(), ...]
 *
 *    Returns the tail of List, that is, the list minus the first element, for
 *    example:
 *
 *    > tl([geesties, guilies, beasties]).
 *    [guilies, beasties]
 *
 *    Allowed in guard tests.
 *
 *    Failure: badarg if List is the empty list [].
*/
bif(lit(atom,erlang),lit(atom,'tl'),[cons(_H,T)], T).
bif(lit(atom,erlang),lit(atom,'tl'),[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= cons(_,_) ).

/** ----------------------------------------------------------------------------
 *  trunc(Number) -> integer()
 *
 *    Types
 *      Number = number()
 *
 *    Returns an integer by truncating Number, for example:
 *
 *    > trunc(5.5).
 *    5
 *
 *    Allowed in guard tests.
*/
bif(lit(atom,erlang),lit(atom,'trunc'),[lit(int,N)], lit(int,N)).
bif(lit(atom,erlang),lit(atom,'trunc'),[lit(float,N)], lit(int,I)) :-
  { N >= 0 }, I #>= 0,
  bif(lit(atom,erlang),lit(atom,'floor'),[lit(float,N)], lit(int,I)).
bif(lit(atom,erlang),lit(atom,'trunc'),[lit(float,N)], lit(int,I)) :-
  { N < 0 }, I #=< -1,
  bif(lit(atom,erlang),lit(atom,'ceil'),[lit(float,N)], lit(int,I)).
bif(lit(atom,erlang),lit(atom,'trunc'),[Arg], error(badarg)) :-
  when(nonvar(Arg), ( Arg \= lit(int,_), Arg \= lit(float,_) ) ).

/** ----------------------------------------------------------------------------
 *  tuple_size(Tuple) -> integer() >= 0
 *
 *    Types
 *      Tuple = tuple()
 *
 *    Returns an integer that is the number of elements in Tuple, for example:
 *
 *    > tuple_size({morni, mulle, bwange}).
 *    3
 *
 *    Allowed in guard tests.
*/
bif(lit(atom,erlang),lit(atom,'tuple_size'),[tuple(T)], lit(int,S)) :-
  S #>= 1,
  when(nonvar(T), length(T,S) ).
bif(lit(atom,erlang),lit(atom,'tuple_size'),[Arg], error(badarg)) :-
  when(nonvar(Arg), Arg \= tuple(_) ).

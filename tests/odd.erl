-module(odd).
-export([odd/1]).

odd(0) -> false;
odd(1) -> true;
odd(N) when N >= 2 -> odd(N-2);
odd(a) -> 0.

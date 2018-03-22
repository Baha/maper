-module(weird_fun).
-export([weird/1]).

weird(0) -> false;
weird(1) -> true;
weird(N) when N+1>=2 -> 12;
weird(_) -> 10.

-module(test).
-export([funcA/0]).

funcA() ->
  funcB().

funcB() ->
  test2:funcC().

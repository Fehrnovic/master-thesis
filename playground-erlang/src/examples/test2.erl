-module(test2).
-export([funcC/0, funcD/0]).

funcC() ->
  test3:funcE().

funcD() ->
  2.

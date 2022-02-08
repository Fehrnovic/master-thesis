-module(test3).
-export([funcE/0]).

funcE() ->
  test2:funcD().

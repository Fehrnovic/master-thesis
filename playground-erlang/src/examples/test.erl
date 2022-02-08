-module(test).
-export([funcA/0]).

funcA() ->
  funcB().

funcB() ->
  2.

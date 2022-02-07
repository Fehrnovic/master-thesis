-module(test).
-export([funcA/0]).

funcA() ->
  funcB(),
  funcC().

funcB() ->
  funcC(),
  2.

funcC() ->
  3.
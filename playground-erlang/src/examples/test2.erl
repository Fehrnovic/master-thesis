-module(test2).
-export([funcC/0]).

funcC() ->
  funcD().

funcD() ->
  2.

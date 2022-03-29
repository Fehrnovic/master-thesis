-module(aaaa).
-export([funcA/1]).

-spec funcA(integer()) -> string().
funcA(_A) ->
  funcB(_A).

%-spec funcB(any()) -> {abe, 5} | {hest, true}.
funcB(A) ->
  A;
funcB(A) ->
  5.

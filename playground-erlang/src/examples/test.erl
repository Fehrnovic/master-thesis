-module(test).
-export([funcA/1]).

-spec funcA(any()) -> cheese.
funcA(A) ->
  case A of
    1 -> funcB(A);
    2 -> funcB(A);
    _ -> 3
  end.

-spec funcB(any()) -> cheese.
funcB(B) ->
  funcA(B + 1).

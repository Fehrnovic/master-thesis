-module(aaaa).
-export([funcA/1]).

funcA(A) ->
  case A of
    5 -> funcB(A);
    _ -> funcB(A)
  end.

%-spec funcB(any()) -> {abe, 5} | {hest, true}.
funcB(A) ->
  case A of
    5 -> 5;
    4 -> "hej";
    _ -> true
  end.

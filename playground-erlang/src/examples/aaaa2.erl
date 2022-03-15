-module(aaaa2).
-export([funcA/0]).

% client APIs
funcA() ->
  _P = funcB(ok),
  funcB(fail).

-spec funcB(any()) -> ok.
funcB(ok) -> 5;
funcB(fail) -> false.
